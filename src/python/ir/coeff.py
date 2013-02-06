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

# container for coefficient object
class Coeff(object): 
    pass
        

# code generators just need to know how to generate these objects
# Matrix, Eye
class Matrix(Coeff):
    """Abstract representation of a matrix parameter coefficient"""
        
    def __init__(self, parameter, transposed=False):
        self.name = parameter.name #[1:]  # removes leading _ used for mangling
        self.transposed = transposed        
        
    def __repr__(self):
        return "Matrix %s" % self.name #[1:] # removes leading _ used for mangling

class Vector(Coeff):
    """Abstract representation of a vector parameter coefficient"""
        
    def __init__(self, parameter, transposed=False):
        self.name = parameter.name #[1:]  # removes leading _ used for mangling
        self.transposed = transposed
        
    def __repr__(self):
        return "Vector %s" % self.name #[1:] # removes leading _ used for mangling

class Scalar(Coeff):
    """Abstract representation of a scalar parameter coefficient"""
        
    def __init__(self, parameter):
        self.name = parameter.name #[1:]
          
    def __repr__(self):
        return "Scalar %s" % self.name #[1:] # removes leading _ used for mangling


class Eye(Coeff):
    """Abstract representation of an identity matrix, alpha*I"""
        
    def __init__(self, n, alpha = 1.0):
        self.n = n
        self.alpha = alpha
        

    def __repr__(self):
        return "%s * I(%s,%s)" % (str(self.alpha), self.n, self.n)
        
class Zero(Coeff):
    """Abstract representation of a zero"""
        
    def __init__(self, n):
        self.n = n        
    
    def __repr__(self):
        return "0(%s)" % self.n

class Ones(Coeff):
    """Abstract representation of a ones vector, alpha*ones"""
    
    def __init__(self, n, alpha = 1.0):
        self.n = n
        self.alpha = alpha
        
    def __repr__(self):
        return "%s * ones(%s)" % (str(self.alpha), self.n)


# hash using test_dict[a.__class__]
test_dict = {
    Coeff: "I'm a coeff",
    Matrix: "I'm a matrix",
    Vector: "I'm a vector",
    Scalar: "I'm a scalar",
    Eye: "I'm identity",
    Zero: "I'm zero!",
    Ones: "I'm all ones"
}