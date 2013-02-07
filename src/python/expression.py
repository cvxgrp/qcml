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

def error_msg(s):
    def err():
        raise Exception(s)
    return err

def id_wrapper(s):
    def id_return():
        return s
    return id_return
    
class Shape(object):
    shapes = set(['MATRIX', 'VECTOR', 'SCALAR'])
    
    add_lookup = {
        ('SCALAR','SCALAR'): id_wrapper('SCALAR'),
        ('SCALAR','VECTOR'): id_wrapper('VECTOR'),
        ('VECTOR','SCALAR'): id_wrapper('VECTOR')
    }
    
    mul_lookup = {
        ('SCALAR','SCALAR'): id_wrapper('SCALAR'),
        ('SCALAR','VECTOR'): id_wrapper('VECTOR'),
        ('VECTOR','SCALAR'): id_wrapper('VECTOR'),
        ('MATRIX','VECTOR'): id_wrapper('VECTOR')
    }
    
    def __init__(self,shape_str):
        if shape_str in self.shapes:
            self.shape_str = shape_str
        else:
            raise Exception("No such shape %s exists." % str(shape_str))
        
    def __repr__(self):
        return "Shape('%s')" % self.shape_str
    
    def __str__(self):
        return self.shape_str
        
    def __add__(self, other):
        lhs = self.shape_str
        rhs = other.shape_str
        
        f = self.add_lookup.get( 
            (lhs, rhs), 
            error_msg("No addition operator implemented for '%s + %s'." % (lhs,rhs)) 
        )
        return Shape(f())
    
    def __sub__(self, other):
        lhs = self.shape_str
        rhs = other.shape_str
        
        f = self.add_lookup.get( 
            (lhs, rhs), 
            error_msg("No subtraction operator implemented for '%s - %s'." % (lhs,rhs)) 
        )
        return Shape(f())
       
    def __mul__(self, other):
        lhs = self.shape_str
        rhs = other.shape_str
        
        f = self.mul_lookup.get( 
            (lhs, rhs), 
            error_msg("No multiply operator implemented for '%s * %s'." % (lhs,rhs)) 
        )
        return Shape(f())
        
    def __neg__(self):
        return self
    
    def __eq__(self,other):
        return self.shape_str is other.shape_str
    
    def __ne__(self,other):
        return self.shape_str is not other.shape_str
        
class Sign(object):
    signs = set(['POSITIVE', 'NEGATIVE', 'UNKNOWN'])
    
    add_lookup = {
        ('POSITIVE','POSITIVE'): 'POSITIVE',
        ('NEGATIVE','NEGATIVE'): 'NEGATIVE'
    }
    
    mul_lookup = {
        ('NEGATIVE','NEGATIVE'): 'POSITIVE',
        ('POSITIVE','POSITIVE'): 'POSITIVE',
        ('NEGATIVE','POSITIVE'): 'NEGATIVE',
        ('POSITIVE','NEGATIVE'): 'NEGATIVE'
    }
    
    negate = {'POSITIVE': 'NEGATIVE', 'NEGATIVE':'POSITIVE', 'UNKNOWN':'UNKNOWN'}
    
    def __init__(self,sign_str):
        if sign_str in self.signs:
            self.sign_str = sign_str
        else:
            raise Exception("No such sign %s exists." % str(sign_str))
        
    def __repr__(self):
        return "Sign('%s')" % self.sign_str
    
    def __str__(self):
        return self.sign_str
        
    def __add__(self, other):
        lhs = self.sign_str
        rhs = other.sign_str
        
        sign = self.add_lookup.get( (lhs, rhs), 'UNKNOWN' )
        return Sign(sign)
    
    def __sub__(self, other):
        lhs = self.sign_str
        rhs = self.negate[other.sign_str]
        
        sign = self.add_lookup.get( (lhs, rhs), 'UNKNOWN' )
        return Sign(sign)
       
    def __mul__(self, other):
        lhs = self.sign_str
        rhs = other.sign_str
        
        sign = self.mul_lookup.get( (lhs, rhs), 'UNKNOWN' )
        return Sign(sign)
        
    def __neg__(self):
        self.sign_str = self.negate[self.sign_str]
        return self
        
    def __eq__(self,other):
        return self.sign_str is other.sign_str
    
    def __ne__(self,other):
        return self.sign_str is not other.sign_str

# the ordering here matters. we use a bitwise OR to accomplish vexity
# inference. 
#   AFFINE (0) | ANYTHING = ANYTHING
#   CONVEX (1) | CONCAVE (2) = NONCONVEX (3)
#   SAME | SAME = SAME

# should i make these classes so i can print them?
AFFINE,CONVEX,CONCAVE,NONCONVEX = range(4)

# these are (mutable) globals, so be careful!
POSITIVE = Sign('POSITIVE')
NEGATIVE = Sign('NEGATIVE')
UNKNOWN = Sign('UNKNOWN')

SCALAR = Shape('SCALAR')
VECTOR = Shape('VECTOR')
MATRIX = Shape('MATRIX')

# bitwise masks (AFFINE|CONVEX) = CONVEX, etc.
def iscvx(e):
    return (e.vexity | CONVEX) is CONVEX
def isccv(e):
    return (e.vexity | CONCAVE) is CONCAVE
def isaff(e):
    return e.vexity is AFFINE

def ispositive(x):
    return x.sign.sign_str is 'POSITIVE'
def isnegative(x):
    return x.sign.sign_str is 'NEGATIVE'
def isunknown(x):
    return x.sign.sign_str is 'UNKNOWN'

def check_eq(x,y):
    return isaff(x) and isaff(y)
def check_leq(x,y):
    return iscvx(x) and isccv(y)
def check_geq(x,y):
    return isccv(x) and iscvx(y)

def build_eq(x,y):
    return "%s - (%s) == 0" % (x.name, y.name)
def build_leq(x,y):
    return "%s - (%s) >= 0" % (y.name, x.name)
def build_geq(x,y):
  return "%s - (%s) >= 0" % (x.name, y.name)  
    
# constraint class
class Constraint(object):
    valid_constraints = set(['EQ', 'LEQ', 'GEQ'])
    constraints = {
        'EQ': (check_eq, build_eq),
        'LEQ': (check_leq, build_leq),
        'GEQ': (check_geq, build_geq)
    }
    
    def __init__(self, lhs, rhs, kind):
        """Creates a constraint object, but also checks DCP"""
        if kind in self.valid_constraints:
            check, build = self.constraints[kind]
            if check(lhs,rhs):
                self.lhs = lhs
                self.rhs = rhs
                self.kind = kind
                self.name = build(lhs,rhs)
            else:
                raise Exception("Cannot have '%s %s %s'" % (lhs.vexity, sym, rhs.vexity))
        else:
            raise Exception("Unknown constraint type %s." % kind)  
    
    def __repr__(self):
        return "Constraint(%s, %s, '%s')" % (self.lhs, self.rhs, self.kind)
    
    def __str__(self):
        return self.name

# Expression evaluator goes here....
class Expression(object):
    """ A linear expression. Always affine. """
    # acutally, i lied.
    #
    # 
    # these can be convex, concave, affine
    # can be positive, negative, unknown
    # vexity = NONCONVEX
    # sign = UNKNOWN
    # shape = SCALAR
    # name = ""
    
    add_lookup = {
        ('COEFF','COEFF'): 'COEFF',
    }
    
    mul_lookup = {
        ('COEFF','COEFF'): id_wrapper('COEFF'),
        ('COEFF','ARGUMENT'): id_wrapper('ARGUMENT')
    }
    
    valid_kinds = set(['COEFF', 'ARGUMENT'])
    
    vexity_lookup = {'AFFINE': AFFINE, 'CONVEX': CONVEX, 'CONCAVE': CONCAVE, 'NONCONVEX': NONCONVEX}
    vexity_names = ['AFFINE', 'CONVEX', 'CONCAVE', 'NONCONVEX']
    
    negate = {AFFINE: AFFINE, CONVEX: CONCAVE, CONCAVE: CONVEX, NONCONVEX: NONCONVEX}
    
    def __init__(self, vexity, sign, shape, name, kind='ARGUMENT'):
        if kind in self.valid_kinds:
            self.vexity = vexity
            self.sign = sign
            self.shape = shape
            self.name = name
            self.kind = kind
        else:
            raise Exception("Cannot construct an expression of kind %s." % kind)
        
        # self.description = name
    
    def __add__(self, other):
        vexity = self.vexity | other.vexity
        sign = self.sign + other.sign
        shape = self.shape + other.shape
        if self.kind is other.kind:
            kind = self.kind
        else:
            kind = 'ARGUMENT'
        
        isconstant = False
        if hasattr(self, 'value') and hasattr(other, 'value'):
            value = self.value + other.value
            name = str(value)
            isconstant = True
        else:
            name = self.name + ' + ' + other.name
        
        result = Expression(vexity, sign, shape, name, kind)
        if isconstant: result.value = value
        return result
    
    def __sub__(self, other):
        vexity = self.vexity | self.negate[other.vexity]
        sign = self.sign - other.sign
        shape = self.shape - other.shape
        if self.kind is other.kind:
            kind = self.kind
        else:
            kind = 'ARGUMENT'
        
        isconstant = False
        if hasattr(self, 'value') and hasattr(other, 'value'):
            value = self.value - other.value
            name = str(value)
            isconstant = True
        else:
            name = self.name + ' - (' + other.name + ')'
        
        result = Expression(vexity, sign, shape, name, kind)
        if isconstant: result.value = value
        return result

    def __mul__(self, other):
        # expressions are always affine expressions
        vexity = AFFINE
        # "other" is always affine (this is just a sanity check)
        if self.vexity is not AFFINE:
            raise Exception("Cannot multiply with non-affine on left.")
            
        if ispositive(self): 
            vexity |= other.vexity
        elif isnegative(self):
            vexity |= self.negate[other.vexity]
        else:
            if other.vexity is not AFFINE:
                vexity = NONCONVEX

        sign = self.sign * other.sign
        shape = self.shape * other.shape
        f = self.mul_lookup.get(
            (self.kind,other.kind),
            error_msg("Cannot multiply (%s) with (%s)." % (self.name, other.name))
        )
        kind = f()
        
        isconstant = False
        if hasattr(self, 'value') and hasattr(other, 'value'):
            value = other.value * self.value
            name = str(value)
            isconstant = True
        else:
            name = self.name + ' * (' + other.name + ')'
        
        result = Expression(vexity, sign, shape, name, kind)
        if isconstant: result.value = value
        return result
        
    def __neg__(self):
        self.vexity = self.negate[self.vexity]
        self.sign = -self.sign
        self.shape = -self.shape
        self.name = '-(' + name + ')'
        if hasattr(self, 'value'):
            self.value = -self.value
            self.name = str(self.value)        
        return self
    
    def __le__(self,other):
        return Constraint(self,other, 'LEQ')
    
    def __ge__(self,other):
        return Constraint(self,other, 'GEQ')
    
    def __eq__(self,other):
        return Constraint(self,other, 'EQ')
        
    __lt__ = None
    __gt__ = None
    __ne__ = None
    
    def __repr__(self):
        return "Expression(%s, %s, %s, %s, %s)" % (self.vexity, self.sign, self.shape, self.name, self.kind)
    
    def __str__(self):
        return self.name

class Variable(Expression):
    def __init__(self, name, shape):
        super(Variable, self).__init__(AFFINE, UNKNOWN, shape, name, 'ARGUMENT')
    
    def __repr__(self):
        return "variable %s %s" % ( str(self.name), str.lower(str(self.shape)) )
    
    def __str__(self):
        return self.name
        
class Parameter(Expression):
    def __init__(self, name, shape, sign):
        super(Parameter, self).__init__(AFFINE, sign, shape, name, 'COEFF')
        
    def __repr__(self):
        if isunknown(self):
            return "parameter %s %s" % ( str(self.name), str.lower(str(self.shape)) )
        else:
            return "parameter %s %s %s" % ( str(self.name), str.lower(str(self.shape)), str.lower(str(self.sign)) )
    
    def __str__(self):
        return self.name
        
class Constant(Expression):
    # value = 0.0
    
    def __init__(self, value):
        self.value = value
        if value >= 0:
            sign = POSITIVE
        else:
            sign = NEGATIVE
        super(Constant, self).__init__(AFFINE, sign, SCALAR, str(value), 'COEFF')
        
    def __repr__(self):
        return str(self.value)
    
    __str__ = __repr__

    
# convex, concave, affine decorators
def convex(fn,*args):
    def wrap(*args):
        e = fn(*args)
        e.vexity |= CONVEX
        return e
    return wrap

def concave(fn,*args):
    def wrap(*args):
        e = fn(*args)
        e.vexity |= CONCAVE
        return e.vexity
    return wrap

def affine(fn,*args):
    def wrap(*args):
        e = fn(*args)
        e.vexity |= AFFINE
        return e
    return wrap



def expand_all_args(fn, *args):
    """Ensures all args are variables"""
    def wrap(*args):
        evaluator = args[0]
        expanded_args = map (evaluator.expand, args[1:])
        e = fn(evaluator, *expanded_args)
        return e
    return wrap

    
def fold_with(f):
    """To help with constant folding"""
    def wrapper(fn, *args):
        def wrap(*args):
            if (all(isinstance(e, Constant) for e in args[1:])):
                arg_vals = map (lambda e: e.value, args[1:])
                e = Constant( f(*arg_vals) )
            else:
                e = fn(*args)
            return e
        return wrap
    return wrapper

# annotations for monotonicity
def increasing(op):
    return op.vexity

def decreasing(op):
    if op.vexity is CONVEX: return CONCAVE
    elif op.vexity is CONCAVE: return CONVEX
    else: return op.vexity

def nonmonotone(op):
    if op.vexity is AFFINE: return AFFINE
    else: return NONCONVEX


# globals...
INCREASING, DECREASING, NONMONOTONE = range(3)

        
    