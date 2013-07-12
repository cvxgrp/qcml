# public utility functions
def isvector(x):
    return x.shape.col == 1 and ismatrix(x)

def isscalar(x):
    return x.shape.row == 1 and isvector(x)

def ismatrix(x):
    return x.shape.num_dimensions <= 2

class Shape(object):
    def __init__(self, dimensions = []):
        """ Initializes an object

            dimensions:
                A list of strings and integers
        """
        self.instantiated = False
        self.__dimensions = dimensions
        self.num_dimensions = len(dimensions)

        self._assign_row()
        self._assign_col()

    def _assign_row(self):
        self.row = 1
        if self.num_dimensions >= 1: self.row = self.__dimensions[0]

    def _assign_col(self):
        self.col = 1
        if self.num_dimensions >= 2: self.col = self.__dimensions[1]

    def size_str(self):
        if not self.instantiated:
            return "%s" % ('*'.join(map(str, self.dimensions)))
        else:
            return "%s" % reduce(lambda x,y: x*y, self.__dimensions, 1)

    def __str__(self):
        return "%s([%s])" % (self.__class__.__name__, ', '.join(map(str, self.dimensions)))

    def eval(self, dimension_dictionary):
        if not self.instantiated:
            try:
                # makes the abstract labels concrete numbers
                self.__dimensions = \
                    [dimension_dictionary.get(k, int(k)) for k in self.__dimensions]
                self._assign_row()
                self._assign_col()
                self.instantiated = True
            except ValueError:
                raise Exception("Couldn't find corresponding dimension definition.")

    def transpose(self):
        # blindly swaps rows and cols, creates a new Shape
        return Shape([self.col, self.row])

    def slice(self, begin, end, dim):
        if not self.instantiated:
            raise ValueError("Cannot slice an abstract dimension.")
        if dim >= self.num_dimensions:
            raise ValueError("Slice dimension exceeds array dimension.")
        current_length = self.__dimensions[dim]

        if end >= current_length:
            raise ValueError("Cannot slice beyond current shape length.")

        # create a new "slice"
        new_dims = list(self.__dimensions)
        new_dims[dim] = end - begin
        return Shape(new_dims)



        # _swap(self.row, self.col)
#         _swap(self.__dimensions[0], self.__dimensions[1])

""" Convenience functions for creating shapes.
"""
def scalar():
    return Shape()

def vector(n):
    return Shape([n])

def matrix(m,n):
    return Shape([m,n])


# from qc_dimension import Dimension
#
# # public utility functions
# def isvector(x):
#     return isinstance(x.shape, Vector)
#
# def isscalar(x):
#     return isinstance(x.shape, Scalar)
#
# def ismatrix(x):
#     return isinstance(x.shape, Matrix)
#
# # _isvector, _isscalar, and _ismatrix are local functions. compare to above
# def _isvector(x):
#     return isinstance(x, Vector) or (_ismatrix(x) and (x.col == 1))
#
# def _isscalar(x):
#     return isinstance(x, Scalar) or (_isvector(x) and (x.row == 1))
#
# def _ismatrix(x):
#     return isinstance(x, Matrix)
#
# # def stack(args):
# #     """ Stacks the shapes.
# #
# #         args
# #             list of shapes to stack
# #     """
# #     def _stack(x,y):
# #         new_dimensions = []
# #         if len(x.dimensions) == len(y.dimensions):
# #             if x.dimensions[-1] == y.dimensions[-1]:
# #                 for (e1,e2) in zip(x.dimensions[0:-1], y.dimensions[0:-1]):
# #                     new_dimensions.append(str(e1) + ' + ' + str(e2))
# #                 return Shape(new_dimensions)
# #             else:
# #                 raise TypeError("Cannot stack '%s' and '%s', since they do not agree in last dimension." % (x, y))
# #         else:
# #             raise TypeError("Cannot stack '%s' and '%s', since they are not the same shape." % (x, y))
# #
# #     return reduce(_stack, args)
#
# class Shape(object):
#     def __init__(self, dimensions):
#         """ Initializes an object
#
#             dimensions:
#                 A list of strings and integers
#         """
#         self.dimensions = dimensions
#
#     __sub__ = None
#     __eq__ = None
#     __ne__ = None
#     __add__ = None
#     __mul__ = None
#
#     def __neg__(self): return self
#
#     def size_str(self):
#         return "%s" % ('*'.join(map(str, self.dimensions)))
#
#     def __str__(self):
#         return "%s(%s)" % (self.__class__.__name__, ', '.join(map(str, self.dimensions)))
#
#
#
#     def transpose(self):
#         raise NotImplementedError("Transpose not implemented for %s" % self)
#
#     def slice(self, begin, end, dim):
#         raise NotImplementedError("Slice not implemented for %s" % self)
#
#
# # XXX / TODO: slicing
#
# class Matrix(Shape):
#     def __init__(self, row, col):
#         self.row = Dimension(row)
#         self.col = Dimension(col)
#         super(Matrix, self).__init__([row, col])
#
#
#     def __add__(self, other):
#         if _isscalar(other):
#             return Matrix(self.row, self.col)
#         elif _ismatrix(other):
#             if self.row == other.row and self.col == other.col:
#                 return Matrix(self.row, self.col)
#             else:
#                 raise TypeError("Cannot add / compare %s and %s; incompatible sizes." % (self, other))
#         else:
#             raise TypeError("No addition / compare operator implemented for '%s + %s'." % (self,other))
#
#     def __mul__(self, other):
#         if _isscalar(other):
#             return Matrix(self.row, self.col)
#         elif _ismatrix(other):
#             if self.col == other.row:
#                 if other.col == 1 and self.row == 1:
#                     return Scalar()
#                 elif other.col == 1:
#                     return Vector(self.row)
#                 else:
#                     return Matrix(self.row, other.col)
#             else:
#                 raise TypeError("Cannot multiply %s and %s; incompatible sizes." % (self, other))
#         else:
#             raise TypeError("No multiply operator implemented for '%s * %s'." % (self,other))
#
#     def size_str(self): return "%s*%s" % (self.row, self.col)
#
#     def transpose(self):
#         return Matrix(self.col, self.row)
#
#     def slice(self, begin, end, dim):
#         if end - begin == 1 and dim == 1:
#             # degrade
#             return Vector(self.row)
#         else:
#             if dim == 0: return Matrix(end-begin, self.col)
#             if dim == 1: return Matrix(self.row, end-begin)
#             raise TypeError("Cannot slice dim = %d in a Matrix." % (dim+1))
#
# class Rank1Matrix(Matrix):
#     """ Class for carrying around Rank 1 matrix. Not sure if it will be useful.
#     """
#     def __init__(self, left, right):
#         self.left = left
#         self.right = right
#         super(Rank1Matrix, self).__init__(left.row, right.col)
#
#     def transpose(self):
#         return Rank1Matrix(self.col, self.row)
#
# class Vector(Matrix):
#     def __init__(self, row):
#         super(Vector, self).__init__(row, 1)
#
#     def __add__(self, other):
#         if _isscalar(other):
#             return Vector(self.row)
#         elif _isvector(other):
#             if self.row == other.row:
#                 return Vector(self.row)
#             else:
#                 raise TypeError("Cannot add / compare %s and %s; incompatible sizes." % (self, other))
#         else:
#             raise TypeError("No addition / compare operator implemented for '%s + %s'." % (self,other))
#
#     __sub__ = __add__
#
#     def __mul__(self, other):
#         # exludes Vector * Matrix (this should never happen, since we won't
#         # have Matrix variables)
#         if _isscalar(other):
#             return Vector(self.row)
#         elif _ismatrix(other):
#             if other.row == 1:
#                 # rank 1 multiply
#                 return Rank1Matrix(self, other)
#             else:
#                 raise TypeError("No multiply operator implemented for '%s * %s'." % (self,other))
#         else:
#             raise TypeError("No multiply operator implemented for '%s * %s'." % (self,other))
#
#     def size_str(self): return "%s" % self.row
#
#     def transpose(self):
#         return Matrix(1, self.row)
#
#     def slice(self, begin, end, dim):
#         if end - begin == 1 and dim == 0:
#             # degrade
#             return Scalar()
#         else:
#             if dim == 0: return Vector(end-begin)
#             raise TypeError("Cannot slice dim = %d in a Matrix." % (dim+1))
#
# class Scalar(Vector):
#     def __init__(self):
#         super(Scalar, self).__init__(1)
#
#     def __add__(self, other):
#         if _isscalar(other):
#             return Scalar()
#         elif _isvector(other):
#             return Vector(other.row)
#         elif _ismatrix(other):
#             return Matrix(other.row, other.col)
#         else:
#             raise TypeError("No addition / compare operator implemented for '%s + %s'." % (self,other))
#
#     __sub__ = __add__
#
#     def __mul__(self, other):
#         if _isscalar(other):
#             return Scalar()
#         elif _isvector(other):
#             return Vector(other.row)
#         elif _ismatrix(other):
#             return Matrix(other.row, other.col)
#         else:
#             raise TypeError("No multiply operator implemented for '%s * %s'." % (self,other))
#
#     def size_str(self): return "1"
#
#     def transpose(self):
#         return self
#
#     def slice(self, begin, end, dim):
#         raise TypeError("Cannot slice a scalar.")
