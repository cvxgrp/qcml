import itertools
from use import use

""" Shape class (and its helpers)
    This exposes the class Shape, and the three Shape constructors: Scalar,
    Vector, and Matrix.

    TODO: Scalar, Vector, and Matrix could be represented as subclasses of
    Shape. This was the original implementation but was rejected because we
    had to catch cases such as (1,n) * (n,1) --> 1.

    Since Shapes are determined by their dimensions (i.e., a (3,1) matrix is
    a vector), it is easier to determine whether something is a matrix,
    vector, or scalar from the dimension data than from the class itself.

    Otherwise, the design of this module ought to be similar to qc_sign and
    qc_vexity.
"""

# public utility functions
@use('shape')
def isvector(x):
    return x.col == 1 and ismatrix(x)

@use('shape')
def isscalar(x):
    return x.row == 1 and isvector(x)

@use('shape')
def ismatrix(x):
    return x.num_dimensions <= 2

# local utility functions
def _strip_trailing_ones(elems):
    elems = [elem for elem in itertools.dropwhile(lambda x: x == 1, reversed(elems))]
    return elems[::-1]  # reverse the list

def _int_or_str(x):
    if isinstance(x,str) and x.isdigit(): return int(x)
    return x

class Shape(object):
    def __init__(self, dimensions = []):
        """ Initializes an object

            dimensions:
                A list of strings and integers
        """
        # ensure that dimensions contains ints if possible
        dimensions = map(_int_or_str, dimensions)
        # if all ints, the shape is already instantiated
        self.instantiated = all(type(elem) == int for elem in dimensions)
        self.dimensions = _strip_trailing_ones(dimensions)
        self.num_dimensions = len(self.dimensions)

        self._assign_row()
        self._assign_col()

    def _assign_row(self):
        self.row = 1
        if self.num_dimensions >= 1: self.row = self.dimensions[0]

    def _assign_col(self):
        self.col = 1
        if self.num_dimensions >= 2: self.col = self.dimensions[1]

    def size(self):
        if self.instantiated:
            return reduce(lambda x,y: x*y, self.dimensions, 1)
        else:
            raise ValueError("Cannot compute size of abstract dimension")

    def __str__(self):
        if ismatrix(self): return "Matrix(%s,%s)" % (self.row, self.col)
        if isvector(self): return "Vector(%s)" % self.row
        if isscalar(self): return "Scalar()"
        return "Shape([%s])" % (', '.join(map(str, self.dimensions)))

    def eval(self, dimension_dictionary):
        # dimension_dictionary is a dictionary of {string: int}
        if not self.instantiated:
            try:
                # makes the abstract labels concrete numbers
                self.dimensions = \
                    [dimension_dictionary.get(k, k) for k in self.dimensions]
                self._assign_row()
                self._assign_col()
                self.instantiated = True
            except ValueError:
                raise Exception("Couldn't find corresponding dimension definition.")
        # returns self so we can chain .eval(...).size(), etc.
        # allows us to do javascript style programming
        return self

    def transpose(self):
        # blindly swaps rows and cols, creates a new Shape
        if ismatrix(self):
            return Shape([self.col, self.row])
        raise ValueError("Cannot transpose non-matrix.")

    def slice(self, begin, end, dim):
        """ XXX: Can only handle concrete slices.
            Abstract slices, such as
                x(0:n-2)
                A(5:n,0:m-n)
            where m,n are dimensions cannot be handled at the moment.

            It is debatable whether we need to support this. We can revisit
            this discussion at a future time.
        """
        if not self.instantiated:
            raise ValueError("Cannot slice an abstract dimension.")
        if dim >= self.num_dimensions:
            raise ValueError("Slice dimension exceeds array dimension.")
        current_length = self.dimensions[dim]

        if end >= current_length:
            raise ValueError("Cannot slice beyond current shape length.")

        # create a new "slice"
        new_dims = list(self.dimensions)
        new_dims[dim] = end - begin
        # drop trailing ones to maintain minimal list
        new_dims = _strip_trailing_ones(new_dims)
        return Shape(new_dims)

    def __eq__(self, other):
        return self.dimensions == other.dimensions

    def __add__(self, other):
        if self == other: return Shape(self.dimensions)
        if isscalar(self): return Shape(other.dimensions)
        if isscalar(other): return Shape(self.dimensions)

        raise TypeError("Cannot add %s and %s; incompatible sizes." % (self, other))


    def __mul__(self, other):
        if not (ismatrix(self) and ismatrix(other)):
            raise TypeError("Cannot multiply non-matrices.")

        if self.col == other.row: return Matrix(self.row, other.col)
        if isscalar(self): return Matrix(other.row, other.col)
        if isscalar(other): return Matrix(self.row, self.col)

        raise TypeError("Cannot multiply %s and %s; incompatible sizes." % (self, other))

    def __neg__(self): return self

""" Convenience functions for creating shapes. Uppercase to "fake" class
    creation.
"""
def Scalar():
    return Shape()

def Vector(n):
    return Shape([n])

def Matrix(m,n):
    return Shape([m,n])
