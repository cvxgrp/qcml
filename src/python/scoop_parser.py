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

from scoop_expression import Operand, Variable, Parameter, Atom, Expression, \
    SCALAR, CONVEX, CONCAVE, AFFINE, iscvx, isccv, isaff
from scoop_atoms import Evaluator
from macro import MacroExpander, is_function

from scoop_tokens import scanner, precedence
from collections import deque
from profiler import profile, print_prof_data  

# state to keep track of whether we are parsing *before* a MINIMIZE,
# MAXIMIZE, or FIND keyword; *on* the line that has that keyword; or
# *after* that line
PRE_OBJ, OBJ, POST_OBJ = range(3)  

# check for agreement of objective vexity with argument
check = \
    {
        "MINIMIZE": iscvx,
        "MAXIMIZE": isccv,
        "FIND": isaff
    }
    
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
        
        self.expander = MacroExpander()
        
        # initialize your scanner, create a new one using current rpn evaluator
        self.scanner = scanner(self.rpn_eval)
                
        # lookup table for tokens and their corresponding operators
        # TODO: decouple ops from instance? (lose ability to build IR)
        # self.tok_op = \
        #     {
        #         "MINUS_OP": self.rpn_eval.sub,
        #         "PLUS_OP": self.rpn_eval.add,
        #         "MULT_OP": self.rpn_eval.mul,
        #         "UMINUS": self.rpn_eval.neg
        #     }
    
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
        return self.scanner.scan(s)
    
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
                #self.rpn_eval.hello()
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
        (tok, val) = toks.popleft()
        if self.state is PRE_OBJ:
            self.state = OBJ
        else:
            raise Exception("Cannot have multiple objectives per SCOOP problem.")
        expr = self.parse_expr(toks)    # expr is an RPN stack, this also checks DCP
        
        # perform macro expansion on the RPN
        stack_of_rpns = self.expander.expand( expr )
        print self.expander.lines
        
        # stack_of_rpns.pop() -- at the top, it's a linear functional
        # you keep popping and chaining DCP rules to the inequalities
        
        # # evaluate the expression
        # obj = eval_rpn(expr)
        # # we expand the objective, which inserts it into our underlying 
        # # problem IR. this is done in case obj is just a single variable
        # obj = self.rpn_eval.expand( obj )
        # if obj:                
        #     if obj.shape is not SCALAR:
        #         raise Exception("Objective function %s should be scalar." % obj.description)
        #     if not check[tok](obj):
        #         raise Exception("Objective vexity %s does not agree with %s" % (Operand.vexity_names[obj.vexity], tok))
        # else:
        #     raise Exception("No objective specified.")
        
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
                # x + (-y) = x - y
                if last_tok is "PLUS_OP":
                    op_stack.pop()
                    op_stack.append(("MINUS_OP", self.rpn_eval.sub))
                    continue
                # x - (-y) = x + y
                if last_tok is "MINUS_OP":
                    op_stack.pop()
                    op_stack.append(("PLUS_OP",self.rpn_eval.add)) 
                    continue
                # -(-y) = y
                if last_tok is "UMINUS":
                    op_stack.pop()
                    continue # -(-x) = x
                # set uminus action
                val = self.rpn_eval.neg
            if tok is "PLUS_OP" and precedence(last_tok) >= 0:
                # this is a unary plus, skip it entirely
                continue
            
            # keep track of old token
            last_tok = tok
            
            if tok is "CONSTANT" or tok is "ONES" or tok is "ZEROS":
                rpn_stack.append((tok,val,0))
            elif tok is "IDENTIFIER" and val in self.symtable:
                paramOrVariable = self.symtable[val]
                rpn_stack.append( (tok,paramOrVariable,0) )
            elif is_function(tok,val):
                # functions have highest precedence
                op_stack.append((tok,val,0))
                argcount_stack.append(0)
            elif tok is "IDENTIFIER":
                raise SyntaxError("\"%(s)s\"\n\tUnknown identifier \"%(val)s\"." % locals())
            elif tok is "COMMA":
                op = ""
                while op is not "LPAREN":                    
                    if op_stack:
                        op,sym,arg = op_stack[-1]   # peek at top
                        if op is not "LPAREN":
                            push_rpn(rpn_stack, argcount_stack, op,sym,arg)
                            op_stack.pop()
                    else:
                        raise SyntaxError("\"%(s)s\"\n\tMisplaced separator or mismatched parenthesis in function %(sym)s." % locals())
                argcount_stack[-1] += 1
            
            elif tok is "UMINUS" or tok is "MULT_OP" or tok is "PLUS_OP" or tok is "MINUS_OP":
                if op_stack:
                    op,sym,arg = op_stack[-1]   # peek at top
                    # pop ops with higher precedence
                    while op_stack and (precedence(tok) < precedence(op) or is_function(op,sym)):
                        op,sym,arg = op_stack.pop()
                        push_rpn(rpn_stack, argcount_stack, op,sym,arg)
                        if op_stack:
                            op,sym,arg = op_stack[-1]   # peek at top
                        #else:
                        #    raise SyntaxError("\"%(s)s\"\n\tInvalid operator %(tok)s application; stuck on %(op)s." % locals())
                if tok is "UMINUS": nargs = 1
                else: nargs = 2
                op_stack.append( (tok, val, nargs) )
            # elif tok is "PLUS_OP" or tok is "MINUS_OP":

            elif tok is "EQ" or tok is "LEQ" or tok is "GEQ":
                # boolean operators have the lowest precedence
                # so pop everything off, until we hit BOOL_OP
                op = ""
                while op is not "BOOL_OP":
                    if op_stack:
                        op,sym,arg = op_stack.pop()
                        if op is not "BOOL_OP": push_rpn(rpn_stack, argcount_stack, op,sym,arg)
                    else:
                        raise SyntaxError("\"%(s)s\"\n\tBoolean expression where none expected." % locals())
                op_stack.append( (tok, val, 2) )

            elif tok is "LPAREN" or tok is "BOOL_OP":
                op_stack.append((tok,val,0))
            elif tok is "RPAREN":
                op = ""

                while op is not "LPAREN":
                    if op_stack: 
                        op,sym,arg = op_stack.pop()
                        if op is not "LPAREN": push_rpn(rpn_stack, argcount_stack, op,sym,arg)
                    else:
                        raise SyntaxError("\"%(s)s\"\n\tCould not find matching left parenthesis." % locals())
            else:
                raise SyntaxError("\"%(s)s\"\n\tUnexpected %(tok)s with value %(val)s." % locals())
            
        while op_stack:
            op,sym,arg = op_stack.pop()
            if op is "LPAREN":
                raise SyntaxError("\"%(s)s\"\n\tCould not find matching right parenthesis." % locals())
            if op is "BOOL_OP":
                raise SyntaxError("\"%(s)s\"\n\tExpected to find boolean constraint." % locals())
            
            push_rpn(rpn_stack, argcount_stack, op,sym,arg)

        if argcount_stack:
            raise Exception("Unknown error. Variable argument functions failed to be properly parsed")
        
        # operators / functions are (# args, func)
        # operands are Variables, Params, or Constants
        return rpn_stack

def push_rpn(stack,argcount,op,sym,arg):
    if is_function(op,sym):
        a = argcount.pop()
        stack.append( (op, sym, a+1) )
    else:
        stack.append( (op,sym,arg) )

def eval_rpn(stack):
    operand_stack = []
    for (tok, op, nargs) in stack:
        print map(lambda e: e.description, operand_stack)
        if isinstance(op, Operand):
            operand_stack.append(op)
        elif tok is "MULT_OP" or tok is "PLUS_OP" or tok is "MINUS_OP":
            rhs = operand_stack.pop()
            lhs = operand_stack.pop()
            operand_stack.append(op(lhs, rhs))
        elif tok is "UMINUS":
            arg = operand_stack.pop()
            operand_stack.append(op(arg))
        else:
            raise Exception("%s %s is not yet implemented." % (tok, op))
    
    print map(lambda e: e.description, operand_stack)
    if len(operand_stack) > 1:
        raise Exception("Error evaluating rpn stack: %s. Completed with %s leftover." %  (stack, operand_stack))
    else:
        if operand_stack:
            return operand_stack[0]
        else:
            return None
