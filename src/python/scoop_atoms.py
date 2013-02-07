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

from expression import SCALAR, VECTOR, MATRIX, Variable, Expression, Parameter, Constant
from ir.dimension import DimSet, Row, Col
from ir.coeff import Vector, Eye, Ones

# this is just a scaffolding for atoms
class Evaluator(object):
    # counter for new variables introduced (private?)
    # varcount = 0
     
    # symtable = {} # i don't actually need a symtable...
    
    def __init__(self):
        # self.symtable = table
        self.tmplist = {}           # dict of temp variables
        self.varlist = {}           # dict of variables used, outputs of solver
        self.paramlist = {}         # dict of parameters used, inputs of solver
        # i only keep the above 3 lists so i can "print" the problem
        self.dimensions = DimSet()  # keeps track of equivalence relation between variable lengths
        self.equiv = {}             # gives variable name and its concrete dimension (used in preprocessing)
        self.varcount = 0
        self.affine = []
        self.cones = []
    
    def hello(self):
        pass #print self.symtable
    
    def new_var(self,shape):
        """Creates a new, temporary variable"""
        name = 't' + str(self.varcount)
        self.varcount += 1
        v = Variable(name, shape)
        self.tmplist[name] = v
        return v
        
    def expand(self, x):
        """Expands parameters and constants"""
        if isinstance(x,Variable):
            # add to varlist
            self.varlist[x.name] = x
            return x
        elif isinstance(x,Expression):
            return x
        else:
            if x.shape is MATRIX:
                raise Exception("Cannot expand %s since it is a matrix." % x.name)
                
            v = self.new_var(x.shape)
            m = v.name
                
            if isinstance(x,Constant):
                row = [(Eye(1),v), (Ones(1,x.value),)]
            elif isinstance(x,Parameter):
                row = [(Eye(m),v), (Vector(x),)]
                self.paramlist[x.name] = x
                self.equiv[v.name] = Row(x)
            else:
                raise Exception("Attempted to expand an unknown type.")
            
            self.affine.append( row )
            return v



# (Coeff, Variable)? (Coeff,)

    # what does it do?
    # every Operand has a .rewrite associated with it
    # a row is
    # [(Parameter, Variable), (Parameter, Variable), .., (Parameter,)]
    # a cone constraint is
    # no, for the cones, we will form
    # (SCALAR Variable, Variable, Variable) # in (t,x,y,z) form
    # (VECTOR Variable, Variable) # this is multiarg form |x|<=y
    # (VECTOR Variable, Variable, Variable) # norms([x y]) <= vec in R^n, norm is in R^2
    # (Variable, ) # this is x >= 0
    # we'll pre-process this list before codegen, which will get us G, h
    # and will also form "struct" directly
        
    # printing is just a matter of printing the symbol table
    # then each *row* of Ax = b
    # then the list of cones
        
    # codegen is just a matter of gen-ing each *row* of Ax = b
    # then converting IR form to G,h, then gen-ing each *row block* of 
    # Gx + s = h
        
    # that will produce a bunch of constraints and give the top-level name
    
# 1) as i parse, any "variables" or "parameters" will get created in the IR symbol table (separate lists)
    
# 2) when Evaluator is run, it first checks to see if any of the special
# cases are satisfied across the *entire* line
# these are:
# [VARIABLE, ZERO, GEQ] -- "variable len" linear cone (SOC 1)
# [VARIABLE, ABS, VARIABLE, LEQ] -- "variable len" # of (SOC 2)
# [VARIABLE, NORM, SCALAR VARIABLE, LEQ] -- "1" # of (SOC VARLEN + 1)
# [VARIABLE, ..., NORM, VARIABLE, LEQ] -- "variable len" # of (SOC # args + 1)
# affine EQ... (how to check this?)
#
# so the usual EYE is
# EYE(start=0,stride=1) generates -> [1 0; 0 1]
# EYE(start=0,stride=2) generates -> [1 0; 0 0; 0 1; 0 0], but
# EYE(start=1,stride=2) generates -> [0 0; 1 0; 0 0; 0 1]
    
# otherwise, it will just consume using an RPN algorithm

# another question: will i have to have the user provide the dimensions for the variables when they call the solver? i should. this would solve a lot of little problems.
# okay, regarding above: here is the solution for languages with closures:
#    p = Scoop()
#    p.eval?/run?(...)
#    f = p.generate()
#    f(x_len=3,y_len=2)(A=1,b=2,c=3)   # this will solve it
# dimensions are passed in using closures. so if variable dimensions change, you have to recall the whole function, but if only problem data changes, then you just have to call the second function.
# so there's a distinction between problem dimensions and data

# for languages without closures, you will have to do
#    f(A=1,b=2,c=3,x_len=3,y_len=2), etc.
# this is for something like C
#
# alternatively, for C, you could run a C-specific generator that requires knowledge of the dimensions before it generates code. that's fine, too. in fact, most C implementations will probably fall under this category. but they will also 
#
# obviously, i could work around this with C++ templating, but, hey, I'm not going to do that.
