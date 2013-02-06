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

from scoop_expression import SCALAR, VECTOR, MATRIX, Variable

# this is just a scaffolding for atoms
class Evaluator(object):
    # counter for new variables introduced (private?)
    # varcount = 0
     
    # symtable = {} # i don't actually need a symtable...
    
    def __init__(self):
        # self.symtable = table
        self.varlist = {}       # dict of variables used
        self.paramlist = {}     # dict of parameters used
        self.dimensions = {}    # dict for variable lengths
        self.varcount = 0
        self.affine = []
        self.cones = []
    
    def hello(self):
        pass #print self.symtable
    
    def new_var(self,shape):
        name = 't' + str(self.varcount)
        self.varcount += 1
        v = Variable(name, shape)
        self.varlist[name] = (v)
        return v

class Row(object):
    pass
    
class Cone(object):

    def __init__(self, t, *args):
        # args are tuples of Variable
        # cone is (t, *args) \in SOC
        # if t is scalar, then this is *one* cone
        # if t is a vector, then these are multiple cones
        if t.shape is SCALAR:
            self.is_multi_cone = False 
        else:
            self.is_multi_cone = True
        
        self.t = t
        
        # if there are no remaining args, it's an LP cone
        if args:
            self.is_LP_cone = False
            self.cone_variables = args
        else:
            self.is_LP_cone = True
            self.cone_variables = ()
    
    def __repr__(self):
        if self.is_LP_cone:
            return self.t.name + ' >= 0'
        elif self.is_multi_cone:
            return 'norm(' + ', '.join(map(lambda x:x.name, self.cone_variables)) + ') <= ' + self.t.name
        else:
            return 'norm([' + '; '.join(map(lambda x:x.name, self.cone_variables)) + ']) <= ' + self.t.name
            
    def to_affine(self):
        """Converts the cone constraint into a row of an affine equality constraint"""
        # what about cone struct?
        pass

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
