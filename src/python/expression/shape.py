from utils import error_msg, id_wrapper

class Shape(object):
    shapes = set(['MATRIX', 'VECTOR', 'SCALAR'])
    
    add_lookup = {
        ('SCALAR','SCALAR'): id_wrapper('SCALAR'),
        ('SCALAR','VECTOR'): id_wrapper('VECTOR'),
        ('VECTOR','SCALAR'): id_wrapper('VECTOR'),
        ('VECTOR','VECTOR'): id_wrapper('VECTOR')
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
            error_msg(TypeError, "'%s + %s' combination is disallowed." % (lhs,rhs)) 
        )
        return Shape(f())
    
    __sub__ = None
    # def __sub__(self, other):
    #     lhs = self.shape_str
    #     rhs = other.shape_str
    #     
    #     f = self.add_lookup.get( 
    #         (lhs, rhs), 
    #         error_msg("No subtraction operator implemented for '%s - %s'." % (lhs,rhs)) 
    #     )
    #     return Shape(f())
       
    def __mul__(self, other):
        lhs = self.shape_str
        rhs = other.shape_str
        
        f = self.mul_lookup.get( 
            (lhs, rhs), 
            error_msg(TypeError, "No multiply operator implemented for '%s * %s'." % (lhs,rhs)) 
        )
        return Shape(f())
        
    def __neg__(self):
        return self
    
    def __eq__(self,other):
        return self.shape_str == other.shape_str
    
    def __ne__(self,other):
        return self.shape_str != other.shape_str

# these are mutable globals, so be careful!
SCALAR = Shape('SCALAR')
VECTOR = Shape('VECTOR')
MATRIX = Shape('MATRIX')