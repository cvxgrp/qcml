from abc import ABCMeta

class Code(object):
    __metclass__ = ABCMeta
    """ Defines an object that can have code generated for any language.
    """

class Just(Code):
    """ Just a single element to loop over.
    """
    def __init__(self, x):
        self.x = x
        super(Just, self).__init__()

class LoopIndex(Code):
    """ Loop over all indices of a matrix in column-major order.
    """
    def __init__(self, matrix, offset, stride):
        self.matrix = matrix
        self.offset = offset
        self.stride = stride
        super(LoopIndex, self).__init__()

class LoopRows(LoopIndex):
    """ Loops over the row indices in a matrix.
    """
    def __init__(self, matrix, offset, stride):
        super(LoopRows, self).__init__(matrix, offset, stride)

class LoopCols(LoopIndex):
    """ Loops over the col indices in a matrix.
    """
    def __init__(self, matrix, offset, stride):
        super(LoopCols, self).__init__(matrix, offset, stride)

class LoopOver(Code):
    """ Loops over the values in a matrix and apply the operation. The
        operation is specified as a format string.
    """
    def __init__(self, matrix, op = "%s"):
        if isinstance(matrix, LoopOver): self.matrix = matrix.matrix
        else: self.matrix = matrix
        self.op = op
        super(LoopOver, self).__init__()

class Range(Code):
    """ Provides a range to loop over.
    """
    def __init__(self, start, end, stride):
        self.start = start
        self.end = end
        self.stride = stride
        super(Range, self).__init__()

class Repeat(Code):
    """ Provides an object to repeat n times.
    """
    def __init__(self, obj, n):
        self.obj = obj
        self.n = n
        super(Repeat, self).__init__()

class Assign(Code):
    """ Assign object on right to object on left.
    """
    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs
        super(Assign, self).__init__()

class NNZ(Code):
    def __init__(self, obj):
        self.obj = obj
        super(NNZ, self).__init__()
