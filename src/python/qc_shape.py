
# isvector, isscalar, and ismatrix are local functions. compare to the ones
# in utils.py
def isvector(x):
    return isinstance(x, Vector) or (ismatrix(x) and (x.col == 1))

def isscalar(x):
    return isinstance(x, Scalar) or (isvector(x) and (x.row == 1))

def ismatrix(x):
    return isinstance(x, Matrix)

class Shape(object):
    def __init__(self, dimensions):
        """ Initializes an object
        
            dimensions:
                A list of strings and integers
        """
        self.dimensions = dimensions
    
    __sub__ = None
    __eq__ = None
    __ne__ = None
    __add__ = None
    __mul__ = None
    
    def __neg__(self): return self

    def __str__(self):
        return "%s(%s)" % (self.__class__.__name__, ', '.join(map(str, self.dimensions)))


# XXX / TODO: slicing

class Matrix(Shape):
    def __init__(self, row, col):
        self.row = row
        self.col = col
        super(Matrix, self).__init__([row, col])
    

    def __add__(self, other):
        if isscalar(other):
            return Matrix(self.row, self.col)
        elif ismatrix(other):
            if self.row == other.row and self.col == other.col:
                return Matrix(self.row, self.col)
            else:
                raise TypeError("Cannot add %s and %s; incompatible sizes." % (self, other))
        else:
            raise TypeError("No addition operator implemented for '%s + %s'." % (self,other))
    
    def __mul__(self, other):
        if isscalar(other):
            return Matrix(self.row, self.col)
        elif ismatrix(other):
            if self.col == other.row:
                if other.col == 1 and self.row == 1:
                    return Scalar()
                elif other.col == 1:
                    return Vector(self.row)
                else:
                    return Matrix(self.row, other.col)
            else:
                raise TypeError("Cannot multiply %s and %s; incompatible sizes." % (self, other))
        else:
            raise TypeError("No multiply operator implemented for '%s * %s'." % (self,other))
            
class Rank1Matrix(Matrix):
    """ Class for carrying around Rank 1 matrix. Not sure if it will be useful.
    """
    def __init__(self, left, right):
        self.left = left
        self.right = right
        super(Rank1Matrix, self).__init__(left.row, right.col)
        
class Vector(Matrix):
    def __init__(self, row):
        super(Vector, self).__init__(row, 1)
        
    def __add__(self, other):
        if isscalar(other):
            return Vector(self.row)
        elif isvector(other):
            if self.row == other.row:
                return Vector(self.row)
            else:
                raise TypeError("Cannot add %s and %s; incompatible sizes." % (self, other))
        else:
            raise TypeError("No addition operator implemented for '%s + %s'." % (self,other))
    
    __sub__ = __add__
            
    def __mul__(self, other):
        # exludes Vector * Matrix (this should never happen, since we won't 
        # have Matrix variables)
        if isscalar(other):
            return Vector(self.row)
        elif ismatrix(other):
            if other.row == 1:
                # rank 1 multiply
                return Rank1Matrix(self, other)
            else:
                raise TypeError("No multiply operator implemented for '%s * %s'." % (self,other))
        else:
            raise TypeError("No multiply operator implemented for '%s * %s'." % (self,other))
        
class Scalar(Vector):
    def __init__(self):
        super(Scalar, self).__init__(1)
        
    def __add__(self, other):
        if isscalar(other):
            return Scalar()
        elif isvector(other):
            return Vector(other.row)
        elif ismatrix(other):
            return Matrix(other.row, other.col)
        else:
            raise TypeError("No addition operator implemented for '%s + %s'." % (self,other))
    
    __sub__ = __add__
            
    def __mul__(self, other):
        if isscalar(other):
            return Scalar()
        elif isvector(other):
            return Vector(other.row)
        elif ismatrix(other):
            return Matrix(other.row, other.col)
        else:
            raise TypeError("No multiply operator implemented for '%s * %s'." % (self,other))
    