# class Shape:
#     SCALAR, VECTOR, MATRIX = range(3)
#     lookup = {"SCALAR": SCALAR, "VECTOR": VECTOR, "MATRIX": MATRIX}
#         
# class Sign:
#     POSITIVE, NEGATIVE, UNKNOWN = range(3)
#     lookup = {"POSITIVE": POSITIVE, "NEGATIVE": NEGATIVE, "UNKNOWN": UNKNOWN}

# the ordering here matters. we use a bitwise OR to accomplish vexity
# inference. 
#   AFFINE (0) | ANYTHING = ANYTHING
#   CONVEX (1) | CONCAVE (2) = NONCONVEX (3)
#   SAME | SAME = SAME
AFFINE,CONVEX,CONCAVE,NONCONVEX = range(4)
POSITIVE,NEGATIVE,UNKNOWN = range(3)
SCALAR,VECTOR,MATRIX = range(3)

# Expression evaluator goes here....
class Operand(object):
    # these can be convex, concave, affine
    # can be positive, negative, unknown
    vexity = NONCONVEX
    sign = UNKNOWN
    shape = SCALAR
    name = ""
    
    sign_lookup = {'POSITIVE': POSITIVE, 'NEGATIVE': NEGATIVE, 'UNKNOWN': UNKNOWN}
    shape_lookup = {'SCALAR': SCALAR, 'VECTOR': VECTOR, 'MATRIX': MATRIX}
    
    shape_names = shape_lookup.keys()
    sign_names = sign_lookup.keys()
    
    def __init__(self, vexity, sign, shape, name):
        self.vexity = vexity
        self.sign = sign
        self.shape = shape
        self.name = name

class Variable(Operand):
    def __init__(self, name, shape):
        super(Variable, self).__init__(AFFINE, UNKNOWN, self.shape_lookup[shape], name)
    
    def __repr__(self):
        return self.shape_names[self.shape] + ' VARIABLE ' + str(self.name) 
    
    __str__ = __repr__  # delete later
        
class Parameter(Operand):
    def __init__(self, name, shape, sign):
        super(Parameter, self).__init__(AFFINE, self.sign_lookup[sign], self.shape_lookup[shape], name)
        
    
    def __repr__(self):
        return self.sign_names[self.sign] + ' ' \
            + self.shape_names[self.shape] + \
             ' PARAMETER ' + str(self.name) 
    
    __str__ = __repr__ # delete later
        
class Constant(Operand):
    value = 0.0
    
    def __init__(self, value):
        self.value = value
        if value >= 0:
            sign = POSITIVE
        else:
            sign = NEGATIVE
        super(Constant, self).__init__(AFFINE, sign, SCALAR, str(value))
        
    def __repr__(self):
        return str(self.value)
    
    __str__ = __repr__

class Expression(Operand):
    def __init__(self, vexity):
        super(Expression, self).__init__(vexity, UNKNOWN, SCALAR, "")


class Atom(object):
    lookup = {'sqrt': 1, 'geo_mean': 1}
    
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


        
    