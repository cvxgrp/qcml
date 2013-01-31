# class Shape:
#     SCALAR, VECTOR, MATRIX = range(3)
#     lookup = {"SCALAR": SCALAR, "VECTOR": VECTOR, "MATRIX": MATRIX}
#         
# class Sign:
#     POSITIVE, NEGATIVE, UNKNOWN = range(3)
#     lookup = {"POSITIVE": POSITIVE, "NEGATIVE": NEGATIVE, "UNKNOWN": UNKNOWN}

# Expression evaluator goes here....

class Variable(object):
    shape = "uninitialized"
    sign = "UNKNOWN"
    
    def __init__(self, shape):
        self.shape = shape
    
    def __repr__(self):
        return str(self.shape) + ' VARIABLE' 
    
    __str__ = __repr__  # delete later
        
class Parameter(object):
    shape = "uninitialized"
    sign = "uninitialized"
    
    def __init__(self, shape, sign="UNKNOWN"):
        self.shape = shape
        self.sign = sign
    
    def __repr__(self):
        return str(self.sign) + ' ' + str(self.shape) + ' PARAMETER' 
    
    __str__ = __repr__ # delete later
        
class Constant(object):
    value = 0.0
    
    def __init__(self, value):
        self.value = value
    
    def __repr__(self):
        return str(self.value)
    
    __str__ = __repr__

class Expression(object):
    pass

class Atom(object):
    lookup = {'sqrt': 1, 'geo_mean': 1}