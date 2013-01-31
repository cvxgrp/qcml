from expression import Variable, Parameter, Atom
from scoop_tokens import scanner
from collections import deque
from profiler import profile, print_prof_data  

class ScoopParser(object): 
    """A simple parser for the SCOOP language"""
    
    # counter for new variables introduced (private?)
    varcount = 0
    # dictionary for symbol table (private?)
    symtable = {}
    # parser STATE (private?)
    state = ""
    # current line (private?)
    line = ""
    
    def lex(self, s):
        """Tokenizes the input string 's'. Uses the hidden re.Scanner function."""
        return scanner.scan(s)
    
    # TODO: add a lexer that checks the grammar and converts expressions to
    # RPN
    def parse(self,toks):
        """Parses the tokenized list. Could use LR parsing, but SCOOP is simple
        enough that expressions are parsed with a shunting-yard algorithm.
        Everything else is just simple keywords.
        """
        
        # four types of statements:
        #    variable declarations
        #    parameter declarations
        #    minimize objective
        #      ("subject to")
        #    constraints
        actions = { 
            'VARIABLE': self.parse_variable, 
            'PARAMETER': self.parse_parameter,
            'SUBJECT_TO': self.parse_subject_to,
            'MINIMIZE': self.parse_objective,
            'MAXIMIZE': self.parse_objective,
            'FIND': self.parse_objective
        }
        
        # if not one of four listed actions, the default is to attempt to
        # parse a constraint
        actions.get(toks[0][0], lambda x: self.parse_constraint(x))(toks)
        # print ' '.join( map(lambda x:str(x[0]), toks) )
        
        
    def display(self,toks):
        print ' '.join( map(lambda x:str(x[1]), toks) )
   
    @profile
    def run(self,s):
        self.line = s
        result, remainder = self.lex(s)
        if result:
            if not remainder:
                self.parse(deque(result))
                #self.display(result)
                # print '\n'
            else:
                raise Exception("Unknown parse error.")
    
    # parsing functions to follow
    def parse_variable(self,toks):
        """variable IDENTIFIER VECTOR|SCALAR"""
        self.state = ""     # reset the parser state
        s = self.line
        toks.popleft()  # already matched variable keyword
        
        t, v = toks.popleft()
        if t is not "IDENTIFIER":
            raise SyntaxError("\"%(s)s\"\n\tExpected an identifier, but got %(t)s with value %(v)s instead." % locals())
        elif v in self.symtable:
            raise Exception("\"%(s)s\"\n\tThe name %(v)s is already in use for a parameter / variable." % locals())
            
        shape, tmp  = toks.popleft()
        if shape is not "VECTOR" and shape is not "SCALAR":
            raise SyntaxError("\"%(s)s\"\n\tExpected a VECTOR or SCALAR shape, but got %(tmp)s instead." % locals())
        
        # if any remaining
        if toks:
            t, tmp = toks.popleft()
            raise SyntaxError("\"%(s)s\"\n\tUnexpected ending for variables with %(t)s token %(tmp)s." % locals())
                
        self.symtable[v] = Variable(shape)
        
    
    def parse_parameter(self,toks):
        """parameter IDENTIFIER VECTOR|SCALAR (POSITIVE|NEGATIVE)"""
        self.state = ""     # reset the parser state
        s = self.line
        toks.popleft()  # already matched parameter keyword
        
        t, v = toks.popleft()
        if t is not "IDENTIFIER":
            raise SyntaxError("\"%(s)s\"\n\tExpected an identifier, but got %(t)s with value %(v)s instead." % locals())
        elif v in self.symtable:
            raise Exception("\"%(s)s\"\n\tThe name %(v)s is already in use for a parameter / variable." % locals())
            
        shape, tmp  = toks.popleft()
        if shape is not "VECTOR" and shape is not "SCALAR" and shape is not "MATRIX":
            raise SyntaxError("\"%(s)s\"\n\tExpected a VECTOR, SCALAR, or MATRIX shape, but got %(tmp)s instead." % locals())
        
        sign = "UNKNOWN"
        if toks:
            sign, tmp = toks.popleft()
            if sign is not "POSITIVE" and sign is not "NEGATIVE":
                raise SyntaxError("\"%(s)s\"\n\tExpected a POSITIVE or NEGATIVE sign, but got %(tmp)s instead." % locals())
                            
            # if any remaining
            if toks:
                t, tmp = toks.popleft()
                raise SyntaxError("\"%(s)s\"\n\tUnexpected ending for parameters with %(t)s token %(tmp)s." % locals())
                
        self.symtable[v] = Parameter(shape, sign)
        
    
    def parse_subject_to(self,toks):
        """(MINIMIZE|MAXIMIZE|FIND) ... [SUBJECT_TO]"""
        toks.popleft()
        if self.state:
            self.state = ""
        else:
            raise Exception("Cannot use optional \"subject to\" keyword without corresponding minimize, maximize, or find.")

    def parse_objective(self,toks):
        """(MINIMIZE|MAXIMIZE|FIND) expr"""
        toks.popleft()
        self.state = "parsing objective"
        expr = self.parse_expr(toks)    # expr is an RPN stack, this also checks DCP
        # then evaluate the expression
        # eval(expr)
        print expr
        
    def parse_constraint(self,toks):
        """expr (EQ|GEQ|LEQ) expr"""
        # push a special token on the top that only gets popped when we
        # meet an (EQ|GEQ|LEQ)
        self.state = ""     # reset the parser state
        toks.appendleft(("BOOL_OP", ""))
        expr = self.parse_expr(toks)    # expr is an RPN stack, this also checks DCP
        # then evaluate the expression
        # eval(expr)
        print expr
    
    def parse_expr(self,toks):
        """ term = CONSTANT 
                 | variable-IDENTIFIER 
                 | parameter-IDENTIFIER 
                 | ONES
                 | ZEROS
            expr = term 
                 | expr PLUS_OP expr 
                 | parameter-IDENTIFIER MULT_OP expr 
                 | expr MINUS_OP expr 
                 | MINUS_OP expr 
                 | LPAREN expr RPAREN 
                 | function-IDENTIFIER LPAREN args RPAREN
                 | BOOL_OP expr == expr 
                 | BOOL_OP expr <= expr 
                 | BOOL_OP expr >= expr
            args = expr [COMMA expr]*
            
        We use a shunting-yard algorithm to return the expression in RPN. A
        separate function will evaluate it for DCP and restricted multiply,
        then apply term rewriting.
        
        A variable-IDENTIFIER, parameter-IDENTIFIER, or function-IDENTIFIER is
        an identifier whose name is in the corresponding symbol table. Special
        keywords like ABS, NORM are function-IDENTIFIERS.
        """
        s = self.line
        rpn_stack = []
        op_stack = []

        while toks:
            # basic Shunting-yard algorithm from wikipedia
            tok, val = toks.popleft()

            if tok is "CONSTANT" or tok is "ONES" or \
               tok is "ZEROS" or (tok is "IDENTIFIER" and val in self.symtable):
                rpn_stack.append((tok,val))
            elif is_function(tok,val):
                op_stack.append((tok,val))
            elif tok is "IDENTIFIER":
                raise SyntaxError("\"%(s)s\"\n\tUnknown identifier \"%(val)s\"." % locals())
            elif tok is "COMMA":
                op = ""
                while op is not "LPAREN":
                    if op_stack:
                        op,sym = op_stack[-1]   # peek at top
                        if op is not "LPAREN": 
                            rpn_stack.append( (op,sym) )
                            op_stack.pop()
                    else:
                        raise SyntaxError("\"%(s)s\"\n\tMisplaced separator or mismatched parenthesis in function %(sym)s." % locals())
            elif tok is "MULT_OP":
                # multiply has the highest precedence, so it unconditionally
                # gets pushed
                op_stack.append( (tok,val) )
            elif tok is "PLUS_OP" or tok is "MINUS_OP":
                if op_stack:
                    op,sym = op_stack[-1]   # peek at top
                    # +,- have the same precedence and are left-associative
                    # * has the higher precedence, so it must get popped
                    # all functions have higher precedence
                    while op is "MULT_OP" or op is "MINUS_OP" or op is "PLUS_OP" or is_function(op,sym):
                        op,sym = op_stack.pop()
                        rpn_stack.append( (op,sym) )
                        op,sym = op_stack[-1]   # peek at top
                    
                op_stack.append( (tok, val) )
                
            elif tok is "EQ" or tok is "LEQ" or tok is "GEQ":
                # boolean operators have the lowest precedence
                # so pop everything off, until we hit BOOL_OP
                op = ""
                while op is not "BOOL_OP":
                    if op_stack:
                        op,sym = op_stack.pop()
                        if op is not "BOOL_OP": rpn_stack.append( (op,sym) )
                    else:
                        raise SyntaxError("\"%(s)s\"\n\tBoolean expression where none expected." % locals())
                op_stack.append( (tok, val) )

            elif tok is "LPAREN" or tok is "BOOL_OP":
                op_stack.append((tok,val))
            elif tok is "RPAREN":
                op = ""

                while op is not "LPAREN":
                    if op_stack: 
                        op,sym = op_stack.pop()
                        if op is not "LPAREN": rpn_stack.append( (op,sym) )
                    else:
                        raise SyntaxError("\"%(s)s\"\n\tCould not find matching left parenthesis." % locals())
            else:
                raise SyntaxError("\"%(s)s\"\n\tUnexpected %(tok)s with value %(val)s." % locals())
                
        while op_stack:
            op,sym = op_stack.pop()
            if op is "LPAREN":
                raise SyntaxError("\"%(s)s\"\n\tCould not find matching right parenthesis." % locals())
            if op is "BOOL_OP":
                raise SyntaxError("\"%(s)s\"\n\tExpected to find boolean constraint." % locals())
            
            rpn_stack.append( (op,sym) )
    
        return rpn_stack

def is_function(tok,val):
    return (tok is "NORM" or tok is "ABS" or (tok is "IDENTIFIER" and val in Atom.lookup))
