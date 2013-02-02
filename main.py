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
from scoop import Scoop, print_prof_data

# just a driver for the problems

# if the lang were embedded in python, could do some crazy stuff
# problem is getting variable names from the symbol table
if __name__ == '__main__':
    print "hello"
    p = Scoop()
    
    # i can parse scoop line by line
    # each "file" is just a code block, within the block i just require
    # that you had the thing defined before
    
    # here's an example of how you could embed this inside python with 
    # strings. kind of cool
    map (p.run, 
    ["# this entire line is a comment!",
     "variable x vector # hello, this is way too cool blah",
     "variable _x vector",
     "parameter A matrix positive",
     "parameter b vector",
     "parameter a scalar",
     "variable t scalar",
     "variable y vector",
     "parameter z scalar",
     "minimize -norm(A*x - b,abs(4*x+b))",
     "subject to",
     "  norm(x,y,z) + -5 <= A*x - -3"
     "#norm(x) <= -3",
     "#a >= 0",
     "#abs(x)+3*x <= t -3",
     "#sqrt(3*x-y) >= abs(z)",
     "#geo_mean(3,1) == sqrt(3*2-b)"])

    # f = p.generate()
    # 
    # g = p.deliteGenerate(Adim=(),,)
    # 
    # f(A,b,a,z)
    # g(A,b,a,z)
    
    print p.symtable
    
    print_prof_data()
