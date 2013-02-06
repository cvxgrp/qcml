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

# equivalence will be an external object
class DimSet(object):
    def __init__(self):
        self.equiv = {}
    
    # creates a dictionary of sets
    # self.equiv['x'] gives the set of all variables equivalent to x's length
    def equate(self, *args):
        s = Set()
        # append all the equivalent sets
        for arg in args:
            s |= self.equiv.get( arg, set() ) 
        s |= ( set(args) )
        
        # set all the old ones to the new set
        for arg in s:
            self.equiv[arg] = s
    
    # def define(self, name, val):
    #     (s,v) = self.equiv.get( name, (Set([name]), None) )
    #     if v:
    #         print "two vals should be equal!"
    #     for elem in s:
    #         self.equiv[elem] = (s, val)
            
    
    def __repr__(self):
        return str(self.equiv)
    
    __str__ = __repr__

# abstract dimension object
class Dimension(object):
    # all Dimension objects have the same equivalence relation
    # to ensure this, only set this list via Dimension.equivalence
    equivalence = {}    

    def __init__(self, dim = None):
        self.dim = dim
        

class Row(object):
    """Abstract representation of a row of a parameter"""
    def __init__(self, parameter):
        self.name = parameter.name
    
class Col(object):
    """Abstract representation of a column of a parameter"""
    def __init__(self, parameter):
        self.name = parameter.name
        

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