class Dimension(object):
    def __init__(self, s):
        self.size = s
    
    def __eq__(self,other):
        return self.size == other.size
    def __ne__(self,other):
        return self.size != other.size
    
class Row(Dimension):
    def __init__(self, s):
        super(Row, self).__init__(s)
    
    def __repr__(self):
        return "Row(%s)" % self.size

class Col(Dimension):
    def __init__(self, s):
        super(Col, self).__init__(s)
    
    def __repr__(self):
        return "Col(%s)" % self.size

class Shape(object):
    def __init__(self,m,n):
            self.rows = m
            self.cols = n
        
    def __repr__(self):
        return "Shape(%s, %s)" % (self.rows, self.cols)
    
    def row_value(self, lookup):
        """Lookup the row value from the lookup table"""
        if self.rows.size == 1:
            return 1
        else:
            return lookup[self.rows.size]
    
    def col_value(self, lookup):
        """Lookup the row value from the lookup table"""
        if self.cols.size == 1:
            return 1
        else:
            return lookup[self.cols.size]
    
    __sub__ = None
    __eq__ = None
    __ne__ = None
    
    def __add__(self, other):
        if isinstance(self, Scalar) and isinstance(other, Scalar):
            return Scalar()
        elif isinstance(self, Scalar) and isinstance(other, Vector):
            return Vector(other.rows)
        elif isinstance(self, Vector) and isinstance(other, Scalar):
            return Vector(self.rows)
        elif isinstance(self, Vector) and isinstance(other, Vector):
            if self.rows.size == 1:
                return Vector(other.rows)
            else:
                # also, self.rows == other.rows
                return Vector(self.rows)
        else:
            lhs = self.__class__.__name__
            rhs = self.__class__.__name__
            raise TypeError("'%s + %s' combination is disallowed." % (lhs, rhs))
    
    def __mul__(self, other):
        if isinstance(self, Scalar) and isinstance(other,Scalar):
            return Scalar()
        elif isinstance(self,Scalar) and isinstance(other,Vector):
            return Vector(other.rows)
        elif isinstance(self,Scalar) and isinstance(other, Matrix):
            return Matrix(other.rows, other.cols)
        elif isinstance(self,Vector) and isinstance(other,Scalar):
            return Vector(self.rows)
        elif isinstance(self,Matrix) and isinstance(other,Scalar):
            return Matrix(self.rows, self.cols)
        elif isinstance(self,Matrix) and isinstance(other,Vector):
            # also, their rows are equal to our cols
            if self.rows.size == 1 and other.cols.size == 1:
                return Scalar()
            else:
                return Vector(self.rows)
        else:
            lhs = self.__class__.__name__
            rhs = self.__class__.__name__
            raise TypeError("No multiply operator implemented for '%s * %s'." % (lhs,rhs))
    
    def __neg__(self):
        return self
        
        
class Scalar(Shape):
    def __init__(self,r = None):
        # just ignores the first argument
        super(Scalar,self).__init__(Row(1), Col(1))
    
    def __repr__(self):
        return "Scalar()"
    
    def __str__(self):
        return "scalar"

class Vector(Shape):
    def __init__(self,r):
        if isinstance(r,str):
            super(Vector,self).__init__(Row(r), Col(1))
        else:
            super(Vector,self).__init__(r, Col(1))
    
    def __repr__(self):
        return "Vector(%s)" % self.rows
    
    def __str__(self):
        return "vector"

# class VectorT(Shape):
#     def __init__(self,r):
#         if isinstance(r,str):
#             super(VectorT,self).__init__(Row(1), Col(r))
#         else:
#             super(VectorT,self).__init__(Row(1), r)
#     
#     def __repr__(self):
#         return "VectorT(%s)" % self.cols
#     
#     def __str__(self):
#         return "vector"
        
        
class Matrix(Shape):
    def __init__(self,r,c = None):
        if isinstance(r,str):
            super(Matrix,self).__init__(Row(r), Col(r))
        elif c:
            super(Matrix,self).__init__(r, c)
        else:
            raise Exception("Cannont create matrix with no columns.")
    
    def __repr__(self):
        return "Matrix(%s,%s)" % (self.rows, self.cols)
    
    def __str__(self):
        return "matrix"
        
# class MatrixT(Shape):
#     def __init__(self,r,c=None):
#         if isinstance(r,str):
#             super(MatrixT,self).__init__(Col(r), Row(r))
#         elif c:
#             super(MatrixT,self).__init__(c, r)
#         else:
#             raise Exception("Cannont create matrix with no columns.")
# 
#     
#     def __repr__(self):
#         return "MatrixT(%s)" % self.cols
#     
#     def __str__(self):
#         return "matrix"
