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

import cvxopt as o
from codegen import mangle
# for embedded in python

def generate(self):
    """This function will make sure to check that all *vector* variables have
    their dimension defined. If dimensions are defined for SCALAR variables, 
    they are ignored."""
    codegen = self.codegen
    used = self.used_syms
    variable_set = codegen.needed_variable_dims(used)
    
    def solver(**kwargs):
        # keyword args expect values to be of type int
        if all(isinstance(e,int) for e in kwargs.values()):
            dims = mangle(kwargs)
            # make sure all keys are subset of needed variable list
            if variable_set.issubset(dims):
                print "codegen success!"
                # print the CVXOPT variable list
                print codegen.variables.keys()
                # walk the problem to figure out how the variables align
                codegen.get_variable_sizes(used)
            else:
                raise Exception("Not all variable dimensions have been specified.")
        else:
            raise Exception("Expected integer arguments for variable lengths.") 
    return solver
    
# TODO: attach generate to Scoop class
# def generate(self, *kwargs):
#     self.
#     
# # eventually will take Evaluator object or some IR as input
#     # do as much of the hard work outside of this function
#     # there's not a lot to be done, but the more you do, the better
#     
#     # this is a compromise with CVX and CVXGEN
#     #   * you won't have to re-parse the problem, but you do have to re-stuff
#     #   * you don't get the speed of CVXGEN, but you get the speed of programming (as in CVX)
#     def f(**kwargs):
#         # check to make sure the provided arguments are in our parameter list
#         # at the moment, this list is just provided by the args to generate
#         if set(args).issubset(kwargs):
#             print "Yay! it works!"
#         else:
#             print "You fail! We expected arguments named %s" % str(args)
#                # this function won't work because you passed the wrong arguments. we're expecting
#             raise Exception("SORRY!")
# 
#     # return the solver function
#    return f