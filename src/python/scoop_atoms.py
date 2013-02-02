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

# this is just a scaffolding for atoms
class Evaluator(object):
    # counter for new variables introduced (private?)
    varcount = 0
    symtable = {} # i don't actually need a symtable...
    
    def __init__(self,table):
        self.symtable = table
    
    def hello(self):
        print self.symtable
        
    
    def new_var(self,shape):
        name = 't' + str(self.varcount)
        self.varcount += 1
        return Variable(name, shape)

# (Coeff, Variable)? (Coeff,)

    # what does it do?
    # every Operand has a .rewrite associated with it
    # a row is
    # [(Parameter, Variable), (Parameter, Variable), .., (Parameter,)]
    # a cone constraint is
    # no, for the cones, we will form
    # (SCALAR Variable, Variable, Variable) # in (t,x,y,z) form
    # (VECTOR Variable, Variable) # this is multiarg form |x|<=y
    # (Variable, ZERO) # this is x >= 0
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