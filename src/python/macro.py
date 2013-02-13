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

from expression import UNKNOWN, CONVEX, POSITIVE, \
    Variable, Expression, Parameter, Constant, Cone, to_scoop
import operator
import re

def is_nontrivial_cone(e):
    return isinstance(e,Cone) and not e.istrivial()

def is_variable(e):
    return isinstance(e,Variable)
    
# this is just a scaffolding for atoms
class MacroExpander(object):
    lookup = {}
    
    def __init__(self, table):
        self.__lookup = {}
        self.symtable = table    # variable symbol table

    def __repr__(self):
        return '\n'.join(self.lines)
    
    # this does macro expansion / rewriting
    # once this is done, the AST (stored in RPN) for the problem is always
    # aff = 0
    # aff \in K
    def expand(self, stack):
        """Expand any macros to a set of new SCOOP lines."""
        operand_stack = []
        constraint_stack = []

        cone_stack = [] # stack of cones
        lines = [] # description of problem
        
        last_bool_tok = ''
        last_bool_arg = None
        # set the current lookup table to your local copy
        MacroExpander.lookup = self.__lookup
                
        def gobble(nargs):
            """Gobble args from RPN stack"""
            args = []
            for _ in range(nargs):
                args.append(operand_stack.pop())
            args.reverse() # RPN is reversed, so this flips it back
            return args
    
        for (tok, op, nargs) in stack:
            if tok == 'CONSTANT' or tok == 'IDENTIFIER':
                operand_stack.append(op)
            elif tok == 'MACRO':
                args = gobble(nargs)
            
                # will fail with multiargs (that's OK for now)
                var, definition = op(*args) 
                if definition:

                    cone_stack += filter(is_nontrivial_cone, definition)
                    varlist = filter(is_variable, definition)
                    for v in varlist:
                        self.symtable[v.name] = v
                    
                    lines += filter(None, map(to_scoop, definition))
                    lines.append("")    # add whitepsace at end
                    
                operand_stack.append( var )
            else:
                # these are all operators
                args = gobble(nargs)
                
                # allow chaining of inequality operators
                if (tok == 'LEQ' or tok == 'GEQ' or tok == 'EQ') and last_bool_arg:
                    if tok != last_bool_tok:
                        raise SyntaxError("Cannot chain %s and %s." % (last_bool_tok, tok))
                    else:
                        args[0] = last_bool_arg
                
                result = op(*args)
                
                if tok == 'LEQ' or tok == 'GEQ' or tok =='EQ':
                    last_bool_arg = args[1]
                    last_bool_tok = tok
                                
                # don't append constraints that are trivial
                if isinstance(result, Cone) and not result.istrivial():
                    # this is for parsing constraints
                    constraint_stack.append( result )
                    cone_stack.append( result )
                    
                operand_stack.append( result )
        
        # store the modified lookup locally (in this object)
        self.__lookup = MacroExpander.lookup
        
                
        # if there are any constraints, we return the constraint stack instead
        if constraint_stack: 
            return (constraint_stack, cone_stack, lines)
        else:
            print operand_stack
            return (operand_stack, cone_stack, lines)


