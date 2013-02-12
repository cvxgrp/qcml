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

#from scoop import Scoop, print_prof_data
from scoop import Scoop
# just a driver for the problems

# if the lang were embedded in python, could do some crazy stuff
# problem is getting variable names from the symbol table
if __name__ == '__main__':
    p = Scoop()
    
    # i can parse scoop line by line
    # each "file" is just a code block, within the block i just require
    # that you had the thing defined before
    
    # here's an example of how you could embed this inside python with 
    # strings. kind of cool
    map (p.run, 
    ["# this entire line is a comment!",
     "variable x # hello, this is way too cool blah",
     "variable _x vector",
     "parameter A matrix positive",
     "parameter b vector",
     "parameter a positive",
     "variable t",
     "variable y",
     "variable t0",
     "parameter z",
     "",
     "minimize a*x",
     "  abs(x) <= 3",
     "  norm_inf(x) <= y",
     "  square(0.5 + a*x - 6) + -5 <= a*x <= a*x",
     "#  max(square(x)+2, pos(t)) <= -norm1(y,t)",
     "#  quad_over_lin(_x, x) - min(y) <= -neg(z)",
     "  norm_inf(_x) <= sqrt(0.8*_x-2)"
     "#norm(x) <= -3",
     "#a >= 0",
     "#abs(x)+3*x <= t -3",
     "#sqrt(3*x-y) >= abs(z)",
     "#geo_mean(3,1) == sqrt(3*2-b)"])
    
    print p
    f = p.generate()
    f(x = 1, _x = 3, y = 1, t0 = 1)
    

    
