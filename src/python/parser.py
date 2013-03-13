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

# if the lang were embedded in python, could do some crazy stuff
# problem is getting variable names from the symbol table

from expression import Variable, Parameter, Expression, \
    Scalar, Vector, Matrix, isscalar, \
    Sign, CONVEX, CONCAVE, AFFINE, iscvx, isccv, isaff
from expression.linfunc import LinearFunc
from codegen import Codegen
from macro import MacroExpander

from tokens import scanner, precedence
from collections import deque
from profiler import profile, print_prof_data

import operator
import re
from datetime import datetime

# state to keep track of whether we are parsing *before* a MINIMIZE,
# MAXIMIZE, or FIND keyword; *on* the line that has that keyword; or
# *after* that line
PRE_OBJ, OBJ, POST_OBJ = range(3)

# check for agreement of objective vexity with argument
check = {
    "MINIMIZE": iscvx,
    "MAXIMIZE": isccv,
    "FIND": isaff
}
build_shape = {
    'SCALAR': Scalar,
    'VECTOR': Vector,
    'MATRIX': Matrix
}

comments = re.compile("#.*")

def unknown_err(line, x):
    raise SyntaxError("\"%s\"\n\tUnknown identifier \"%s\"." % (line, x))

class Scoop(object): 
    """A simple parser for the SCOOP language""" 
    
    # global Scoop object variable counter
    varcount = 0
    
    def __init__(self):
        # dictionary for symbol table (private?)
        self.symtable = {}
        # parser STATE (private?)
        self.state = PRE_OBJ
        # current line (private?)
        self.line = ""
        
        # string description of problem
        self.used_syms = set()
        self.description = []
        
        # SOCP data structures (used for codegen)
        self.codegen = Codegen()
        # self.variables = {} # these are used variables
        # self.cones = []
        # self.obj = None
    
        # symtable is passed by reference
        self.expander = MacroExpander(self.codegen.variables)
        
        # private variable counter
        self.__varcount = 0
        
        
    def __str__(self):
        lines = [
            "# " + 70*"=",
            "# Canonicalized SOCP. Converted on %s." % datetime.now().ctime(),
            "#",
            "# Original variable names are mangled and have an underscore ('_')",
            "# preceding them.", 
            "# " + 70*"=",
            ""
        ]
        original_ids = list(self.symtable[e].scoop() for e in self.used_syms)
        lines += original_ids + self.description

        return '\n'.join(lines)
        #return str(self.cones) + str(self.obj)
    
    def __reset_state(self):  # private
        if self.state is OBJ:
            self.state = POST_OBJ
    
    def clear(self):
        """Clears the parser state. Loses all information."""
        self.__reset_state()
        self.symtable = {}
        self.line = ""
        self.varcount = 0
        self.state = PRE_OBJ
    
    def lex(self, s, mangle = True):
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
        
        # if not one of the listed actions, the default is to attempt to
        # parse a constraint
        actions.get(toks[0][0], self.parse_constraint)(toks)        
        
    # def display(self,toks):
    #     print ' '.join( map(lambda x:str(x[1]), toks) )
    
    #@profile
    def run(self, s):
        s.lstrip().rstrip()     # strip any leading or trailing whitespace
        Scoop.varcount = self.__varcount    # set global counter
        LinearFunc.used_vars = self.used_syms  # set the dict for linfunc
        
        self.line = re.sub(comments, "", s.lstrip())
        result, remainder = self.lex(s)
        if result:
            if not remainder:
                self.parse(deque(result))
            else:
                raise Exception("Unknown parse error.")
        
        self.__varcount = Scoop.varcount    # save previous counter
    
    def rewrite(self, s):
        lines = s.split('\n')
        map(self.run, lines)
        
    
    # parsing functions to follow
    def parse_variable(self,toks):
        """variable IDENTIFIER (VECTOR|SCALAR)"""
        self.__reset_state()     # reset the parser state
        s = self.line
        toks.popleft()  # already matched variable keyword
        
        if toks:
            t, v = toks.popleft()
            
            if t != 'IDENTIFIER':
                raise SyntaxError("\"%(s)s\"\n\tExpected an identifier, but got %(t)s with value %(v)s instead." % locals())
            elif v in self.symtable:
                raise Exception("\"%(s)s\"\n\tThe name %(v)s is already in use for a parameter / variable." % locals())
        
            shape = 'SCALAR'
            # optionally parse shape
            if toks:
                shape, tmp  = toks.popleft()
                if shape != 'VECTOR' and shape != 'SCALAR':
                    raise SyntaxError("\"%(s)s\"\n\tExpected a VECTOR or SCALAR shape, but got %(tmp)s instead." % locals())
        
                # if any remaining
                if toks:
                    t, tmp = toks.popleft()
                    raise SyntaxError("\"%(s)s\"\n\tUnexpected ending for variables with %(t)s token %(tmp)s." % locals())
            
            self.symtable[v] = Variable(v,build_shape[shape](v))
        else:
            raise SyntaxError("\"%(s)s\"\n\tNo variable name provided." % locals())
        
    
    def parse_parameter(self,toks):
        """parameter IDENTIFIER (MATRIX|VECTOR|SCALAR) (POSITIVE|NEGATIVE)"""
        self.__reset_state()     # reset the parser state
        s = self.line
        toks.popleft()  # already matched parameter keyword
        
        if toks:
            t, v = toks.popleft()
            
            if t != 'IDENTIFIER':
                raise SyntaxError("\"%(s)s\"\n\tExpected an identifier, but got %(t)s with value %(v)s instead." % locals())
            elif v in self.symtable:
                raise Exception("\"%(s)s\"\n\tThe name %(v)s is already in use for a parameter / variable." % locals())
            
            shape = 'SCALAR'
            sign = 'UNKNOWN'
            
            # optionally parse shape and sign
            if toks:
                token, tmp  = toks.popleft()
                if token == 'POSITIVE' or token == 'NEGATIVE':
                    sign = token
                    look_for_sign = False
                    look_for_shape = True
                elif token == 'VECTOR' or token == 'SCALAR' or token == 'MATRIX':
                    shape = token
                    look_for_sign = True
                    look_for_shape = False
                else:
                    raise SyntaxError("\"%(s)s\"\n\tExpected a shape or sign, but got %(tmp)s instead." % locals())
                    
                
                # optionally parse sign
                if toks:
                    token, tmp  = toks.popleft()
                    if look_for_sign and (token == 'POSITIVE' or token == 'NEGATIVE'):
                        sign = token
                    elif look_for_shape and (token == 'VECTOR' or token == 'SCALAR' or token == 'MATRIX'):
                        shape = token
                    else:
                        raise SyntaxError("\"%(s)s\"\n\tExpected a shape or sign to follow, but got %(tmp)s instead." % locals())
                        
                    # if any remaining
                    if toks:
                        t, tmp = toks.popleft()
                        raise SyntaxError("\"%(s)s\"\n\tUnexpected ending for parameters with %(t)s token %(tmp)s." % locals())
        
            self.symtable[v] = Parameter(v, build_shape[shape](v), Sign(sign))
        else:
            raise SyntaxError("\"%(s)s\"\n\tNo parameter name provided." % locals())
             
    
    def parse_subject_to(self,toks):
        """(MINIMIZE|MAXIMIZE|FIND) ... [SUBJECT_TO]"""
        toks.popleft()
        if self.state is OBJ:
            self.state = POST_OBJ
        else:
            raise Exception("Cannot use optional \"subject to\" keyword without corresponding minimize, maximize, or find.")

    def parse_objective(self, toks):
        """(MINIMIZE|MAXIMIZE|FIND) expr"""
        (tok, val) = toks.popleft()

        if self.state != PRE_OBJ:
            raise Exception("Cannot have multiple objectives per SCOOP problem.")
        # expr is an RPN stack, this also checks DCP
        expr = self.parse_expr(toks)
            
        if not expr:
            raise Exception("No objective specified.")
        
        # perform macro expansion on the RPN
        (obj_stack, cones, new_lines) = self.expander.expand( expr )

        if not obj_stack:   # should never happen
            raise Exception("No objective parsed.")
        obj = obj_stack.pop()
        if obj_stack:       # should never happen
            raise Exception("Unparsed operands.")
                
        if not isscalar(obj.shape):
            raise Exception("\"%s\"\n\tObjective function %s should be scalar." % (self.line, obj.name))
        if not check[tok](obj):
            raise Exception(
                "\"%s\"\n\tObjective vexity %s does not agree with %s" % 
                (self.line, Expression.vexity_names[obj.vexity], tok)
            )
                    
        # label the top
        self.description  += ["",
            "# " + 70*"=",
            "# \"%s\"" % self.line,
            "# " + 70*"="]
                    
        # add the objective
        self.description += new_lines
        self.description += ["# \"%s\"" % self.line,
             "%s %s" % (val, obj.name)]
        # convert to minimization prob
        if tok == 'MAXIMIZE': self.codegen.obj = -obj   
        else: self.codegen.obj = obj
        
        # add the constraints to the cone
        self.codegen.cones += cones

        # set the state to "we just parsed the objective"
        self.state = OBJ
        
    def parse_constraint(self, toks):
        """expr (EQ|GEQ|LEQ) expr"""
        self.__reset_state()                     # reset the parser state
        # start parsing booleans
        # expr is an RPN stack, this also checks DCP
        expr = self.parse_expr(toks, parse_constr = True)    
        if not expr:
            raise Exception("Unknown constraint error.")
                
        # perform macro expansion on the RPN
        (constraint, cones, new_lines) = self.expander.expand( expr )
        # label the top
        self.description += ["",
            "# " + 70*"=",
            "# \"%s\"" % self.line,
            "# " + 70*"="]
                    
        # add the constraint
        self.description += new_lines
        self.description += ["# \"%s\"" % self.line] + map(str, constraint)
        
        self.codegen.cones += cones
                       
    
    def parse_expr(self,toks,parse_constr = False):
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
        is_function_call = False
        
        # set the number of booleans we expect to find
        if parse_constr: expected_bools = 1 
        else: expected_bools = 0

        last_tok = ""

        while toks:
            # basic Shunting-yard algorithm from wikipedia
            # modified to handle variable args
            tok, val = toks.popleft()
            
            if is_function_call and tok != 'LPAREN':
                raise SyntaxError("\"%(s)s\"\n\tExpecting function call but got \"%(tok)s\" (%(val)s) instead." % locals())
            
            # pre-process for unary operators            
            if tok == 'PLUS_OP' and precedence(last_tok) >= 0:
                # this is a unary plus, skip it entirely
                continue
                
            # if "-" follows anything that isn't another operation, then 
            # insert a plus operation
            if tok == 'UMINUS' and precedence(last_tok) < 0:
                toks.appendleft(('UMINUS',operator.neg))
                tok = 'PLUS_OP'
                val = operator.add
            
            # TODO: rewrite this ridculous tree as a dictionary?
            
            if tok == 'CONSTANT':
                rpn_stack.append((tok,val,0))
            elif tok == 'IDENTIFIER':
                
                paramOrVariable = self.symtable.get(val, None)
                if not paramOrVariable: raise SyntaxError("\"%s\"\n\tUnknown identifier \"%s\"." % (s, val))
                                
                # keep track of identifiers we've used (this is in the
                # original problem). since we introduce new variables, i 
                # don't want them "shown" at the end. this is just a separate 
                # data structure to keep track of variables referened in the 
                # original problem.
                self.used_syms.add(val)
                
                
                # keep track of minimal variable and param references (in the rewritten problem)
                if isinstance(paramOrVariable,Variable):
                    self.codegen.variables[val] = paramOrVariable
                else:
                    self.codegen.parameters[val] = paramOrVariable
                
                rpn_stack.append( (tok,paramOrVariable,0) )
            elif tok == 'MACRO':
                # functions have highest precedence
                is_function_call = True
                op_stack.append((tok,val,0))
                argcount_stack.append(0)                
            elif tok == 'COMMA':
                # same as semicolon. TODO: merge the two
                op = ""
                while op != 'LPAREN':                    
                    if op_stack:
                        op,sym,arg = op_stack[-1]   # peek at top
                        if op == 'LBRACE':
                            raise SyntaxError("\"%(s)s\"\n\tCannot use comma separator inside concatenation." % locals())
                        if op != 'LPAREN':
                            push_rpn(rpn_stack, argcount_stack, op,sym,arg)
                            op_stack.pop()
                    else:
                        raise SyntaxError("\"%(s)s\"\n\tMisplaced comma separator or mismatched parenthesis when parsing %(op)s." % locals())
                argcount_stack[-1] += 1
            elif tok == 'SEMI':
                # same as comma. TODO: merge the two
                op = ""
                while op != 'LBRACE':                    
                    if op_stack:
                        op,sym,arg = op_stack[-1]   # peek at top
                        if op == 'LPAREN':
                            raise SyntaxError("\"%(s)s\"\n\tCannot use semicolon separator inside function call." % locals())
                        if op != 'LBRACE':
                            push_rpn(rpn_stack, argcount_stack, op,sym,arg)
                            op_stack.pop()
                    else:
                        raise SyntaxError("\"%(s)s\"\n\tMisplaced semicolon separator or mismatched brace when parsing %(op)s." % locals())
                argcount_stack[-1] += 1
            elif tok == 'TRANSPOSE':
                if last_tok == 'IDENTIFIER' or last_tok == 'RPAREN':
                    rpn_stack.append( (tok, val, 1) )
                else:
                    raise SyntaxError("\"%(s)s\"\n\tCannot transpose %(last_tok)s." % locals())
            elif tok == 'UMINUS' or tok == 'MULT_OP' or tok == 'PLUS_OP': 
                if op_stack:
                    op,sym,arg = op_stack[-1]   # peek at top
                    
                    # pop ops with higher precedence
                    while op_stack and (precedence(tok) < precedence(op)):
                        op,sym,arg = op_stack.pop()
                        push_rpn(rpn_stack, argcount_stack, op,sym,arg)
                        if op_stack:
                            op,sym,arg = op_stack[-1]   # peek at top
                        #else:
                        #    raise SyntaxError("\"%(s)s\"\n\tInvalid operator %(tok)s application; stuck on %(op)s." % locals())
                if tok == 'UMINUS': nargs = 1
                else: nargs = 2
                op_stack.append( (tok, val, nargs) )
            # elif tok is "PLUS_OP" or tok is "MINUS_OP":

            elif tok == 'EQ' or tok == 'LEQ' or tok == 'GEQ':
                if not parse_constr:
                    raise SyntaxError("\"%(s)s\"\n\tBoolean expression where none expected." % locals())
                # boolean operators have the lowest precedence
                # so pop everything off
                op = ""
                while op_stack:
                    op,sym,arg = op_stack.pop()
                    push_rpn(rpn_stack, argcount_stack, op,sym,arg)

                op_stack.append( (tok, val, 2) )
                expected_bools -= 1

            elif tok == 'LPAREN':
                op_stack.append((tok,val,0))
                # we're now in the function call
                if is_function_call: is_function_call = False
            elif tok == 'RPAREN':
                op = ""
                while op != 'LPAREN':
                    if op_stack: 
                        op,sym,arg = op_stack.pop()
                        if op != 'LPAREN': push_rpn(rpn_stack, argcount_stack, op,sym,arg)
                    else:
                        raise SyntaxError("\"%(s)s\"\n\tCould not find matching left parenthesis." % locals())
            elif tok == 'LBRACE':
                # TODO: no action yet
                op_stack.append(('CONCAT', "concatenation!", 0))    
                argcount_stack.append(0)
                op_stack.append((tok,val,0))
            elif tok == 'RBRACE':
                op = ""
                while op != 'LBRACE':
                    if op_stack: 
                        op,sym,arg = op_stack.pop()
                        if op != 'LBRACE': push_rpn(rpn_stack, argcount_stack, op,sym,arg)
                    else:
                        raise SyntaxError("\"%(s)s\"\n\tCould not find matching left brace." % locals())
            else:
                raise SyntaxError("\"%(s)s\"\n\tUnexpected %(tok)s with value %(val)s." % locals())
                
            # keep track of old token
            last_tok = tok
            
        while op_stack:
            op,sym,arg = op_stack.pop()
            if op == 'LPAREN':
                raise SyntaxError("\"%(s)s\"\n\tCould not find matching right parenthesis." % locals())
            if expected_bools > 0:
                raise SyntaxError("\"%(s)s\"\n\tExpected to find boolean constraint." % locals())
            
            push_rpn(rpn_stack, argcount_stack, op,sym,arg)

        if argcount_stack:
            raise Exception("Unknown error. Variable argument functions failed to be properly parsed")
        
        # operators / functions are (# args, func)
        # operands are Variables, Params, or Constants
        return rpn_stack

def push_rpn(stack,argcount,op,sym,arg):
    if op == 'MACRO' or op == 'CONCAT':
        a = argcount.pop()
        stack.append( (op, sym, a+1) )
    else:
        stack.append( (op,sym,arg) )

