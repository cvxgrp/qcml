"""
Copyright (c) 2012-2013, Eric Chu (eytchu@gmail.com)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met: 

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. 
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are
those of the authors and should not be interpreted as representing official
policies, either expressed or implied, of the FreeBSD Project.
"""

from scoop_expression import Variable, Parameter, Atom, Expression
from scoop_atoms import Evaluator
from scoop_tokens import scanner, precedence
from collections import deque
from profiler import profile, print_prof_data  

# state to keep track of whether we are parsing *before* a MINIMIZE,
# MAXIMIZE, or FIND keyword; *on* the line that has that keyword; or
# *after* that line
PRE_OBJ, OBJ, POST_OBJ = range(3)  

class Scoop(object): 
    """A simple parser for the SCOOP language""" 
    
    def __init__(self):
        # dictionary for symbol table (private?)
        self.symtable = {}
        # parser STATE (private?)
        self.state = PRE_OBJ
        # current line (private?)
        self.line = ""
    
        # (stateful) object to evaluate expressions
        self.rpn_eval = Evaluator()  # symtable is passed by reference
    
    def reset_state(self):  # private
        if self.state is OBJ:
            self.state = POST_OBJ
    
    def clear(self):
        """Clears the parser state. Loses all information."""
        self.reset_state()
        self.symtable = {}
        self.line = ""
    
    def lex(self, s):
        """Tokenizes the input string 's'. Uses the hidden re.Scanner function."""
        return scanner.scan(s)
    
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
                self.rpn_eval.hello()
                #self.display(result)
                # print '\n'
            else:
                raise Exception("Unknown parse error.")
    
    # parsing functions to follow
    def parse_variable(self,toks):
        """variable IDENTIFIER VECTOR|SCALAR"""
        self.reset_state()     # reset the parser state
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
        
        # mangle the variable name
        self.symtable[v] = Variable('_' + v,Variable.shape_lookup[shape])  
        
    
    def parse_parameter(self,toks):
        """parameter IDENTIFIER VECTOR|SCALAR (POSITIVE|NEGATIVE)"""
        self.reset_state()     # reset the parser state
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
        # optionally parse sign
        if toks:
            sign, tmp = toks.popleft()
            if sign is not "POSITIVE" and sign is not "NEGATIVE":
                raise SyntaxError("\"%(s)s\"\n\tExpected a POSITIVE or NEGATIVE sign, but got %(tmp)s instead." % locals())
                            
            # if any remaining
            if toks:
                t, tmp = toks.popleft()
                raise SyntaxError("\"%(s)s\"\n\tUnexpected ending for parameters with %(t)s token %(tmp)s." % locals())
        
        # mangle parameter name
        self.symtable[v] = Parameter('_' + v, Parameter.shape_lookup[shape], Parameter.sign_lookup[sign]) 
        
    
    def parse_subject_to(self,toks):
        """(MINIMIZE|MAXIMIZE|FIND) ... [SUBJECT_TO]"""
        toks.popleft()
        if self.state is OBJ:
            self.state = POST_OBJ
        else:
            raise Exception("Cannot use optional \"subject to\" keyword without corresponding minimize, maximize, or find.")

    def parse_objective(self,toks):
        """(MINIMIZE|MAXIMIZE|FIND) expr"""
        toks.popleft()
        if self.state is PRE_OBJ:
            self.state = OBJ
        else:
            raise Exception("Cannot have multiple objectives per SCOOP problem.")
        expr = self.parse_expr(toks)    # expr is an RPN stack, this also checks DCP
        # then evaluate the expression
        # eval(expr)
        print expr
        
    def parse_constraint(self,toks):
        """expr (EQ|GEQ|LEQ) expr"""
        self.reset_state()     # reset the parser state
        # push a special token on the top that only gets popped when we
        # meet an (EQ|GEQ|LEQ)
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
        argcount_stack = []

        last_tok = ""
        while toks:
            # basic Shunting-yard algorithm from wikipedia
            # modified to handle variable args
            tok, val = toks.popleft()
            
            # pre-process for unary operators
            # idea taken from
            # http://en.literateprograms.org/Shunting_yard_algorithm_%28Python%29#Operators
            # unary operator is the first token or any operator preceeded by 
            # another operator
            if tok is "MINUS_OP" and precedence(last_tok) >= 0:
                tok = "UMINUS"
                # x + (-y)
                if last_tok is "PLUS_OP":
                    op_stack.pop()
                    op_stack.append(("MINUS_OP", "-"))
                    continue
                # x - (-y)
                if last_tok is "MINUS_OP":
                    op_stack.pop()
                    op_stack.append(("PLUS_OP","+")) # x - (-y) = x + y
                    continue
                # -(-y)
                if last_tok is "UMINUS":
                    op_stack.pop()
                    continue # -(-x) = x
            if tok is "PLUS_OP" and precedence(last_tok) >= 0:
                # this is a unary plus, skip it entirely
                continue
                
            last_tok = tok
            
            if tok is "CONSTANT" or tok is "ONES" or tok is "ZEROS":
                rpn_stack.append((tok,val))
            elif tok is "IDENTIFIER" and val in self.symtable:
                paramOrVariable = self.symtable[val]
                rpn_stack.append((paramOrVariable.__class__.__name__,(val,paramOrVariable)))
            elif is_function(tok,val):
                # functions have highest precedence
                op_stack.append((tok,val))
                argcount_stack.append(0)
            elif tok is "IDENTIFIER":
                raise SyntaxError("\"%(s)s\"\n\tUnknown identifier \"%(val)s\"." % locals())
            elif tok is "COMMA":
                argcount_stack[-1] += 1
                op = ""
                while op is not "LPAREN":
                    if op_stack:
                        op,sym = op_stack[-1]   # peek at top
                        if op is not "LPAREN":
                            push_rpn(rpn_stack, argcount_stack, op,sym)
                            op_stack.pop()
                    else:
                        raise SyntaxError("\"%(s)s\"\n\tMisplaced separator or mismatched parenthesis in function %(sym)s." % locals())
            elif tok is "UMINUS" or tok is "MULT_OP" or tok is "PLUS_OP" or tok is "MINUS_OP":
                if op_stack:
                    op,sym = op_stack[-1]   # peek at top
                    # pop ops with higher precedence
                    while precedence(tok) < precedence(op) or is_function(op,sym):
                        op,sym = op_stack.pop()
                        push_rpn(rpn_stack, argcount_stack, op,sym)
                        if op_stack:
                            op,sym = op_stack[-1]   # peek at top
                        else:
                            raise SyntaxError("\"%(s)s\"\n\tInvalid operator %(tok)s application; stuck on %(sym)s." % locals())
                
                op_stack.append( (tok, val) )
            # elif tok is "PLUS_OP" or tok is "MINUS_OP":

            elif tok is "EQ" or tok is "LEQ" or tok is "GEQ":
                # boolean operators have the lowest precedence
                # so pop everything off, until we hit BOOL_OP
                op = ""
                while op is not "BOOL_OP":
                    if op_stack:
                        op,sym = op_stack.pop()
                        if op is not "BOOL_OP": push_rpn(rpn_stack, argcount_stack, op,sym)
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
                        if op is not "LPAREN": push_rpn(rpn_stack, argcount_stack, op,sym)
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
            
            push_rpn(rpn_stack, argcount_stack, op,sym)

        if argcount_stack:
            raise Exception("Unknown error. Variable argument functions failed to be properly parsed")
        
        # operators / functions are (# args, func)
        # operands are Variables, Params, or Constants
        return rpn_stack

def is_function(tok,val):
    return (tok is "NORM" or tok is "ABS" or (tok is "IDENTIFIER" and val in Atom.lookup))

def push_rpn(stack,argcount,op,sym):
    if is_function(op,sym):
        a = argcount.pop()
        stack.append((op, sym + str(a+1)))
    else:
        stack.append((op,sym))