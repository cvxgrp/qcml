# define code generation objects
# TODO: can change behavior of these by changing __str__ definition
""" CoeffExpr object.
    Used to construct coefficients of affine expressions.

    Allows us to create abstract objects to represent OnesCoeff, EyeCoeff, etc. and
    perform basic addition, multiplication, etc. on them. Assumes that the
    coefficient math has already passed DCP checking.

    The behavior can be changed by redefining __str__ definition. These classes
    are *final*. You should not subclass them (undefined behavior will occur).

    Note that by setting the __str__ function, e.g., OnesCoeff.__str__ = some_func,
    it is a *global* change to all OnesCoeff objects.

    To allow interchangeability with different codegens, the __str__ function
    is set in the __init__() function of subclasses of the Codegen object.

    However, this means that *two* Codegen objects cannot exist simultaneously,
    as the __str__ function on, say, OnesCoeff, is overwritten by the latest
    Codegen object.

    This is not an issue as long as code generators are only created to generate
    the code and not stored for later use.

    TODO: Possibly figure out how to redefine __str__ without this happening.
    TODO: Or, better, figure out how to modify operators to return the appropriate
        subclass.

    Neither approaches above seem very likely.

    TODO: AddCoeff test cases to CoeffExpr
"""
class CoeffExpr(object):
    # operations only occur on objects with the same shape
    def __add__(self, other): return codegen_add(self, other)

    def __sub__(self, other): return self + (-other)

    def __neg__(self): return codegen_negate(self)

    def __mul__(self,other): return codegen_mul(self, other)

    def trans(self): return codegen_transpose(self)

    # only used during code generation
    # for vertical slicing
    def slice(self, begin, end): return codegen_slice(self, begin, end)

    def to_sparse(self): return ""
    def I(self, row_offset, stride=1): return ""
    def J(self, col_offset, stride=1): return ""
    def V(self): return ""

    def __repr__(self): return str(self)

class ConstantCoeff(CoeffExpr):
    def __init__(self, value):
        self.value = value
        self.isknown = True
        self.isscalar = True
        self.is_matrix_param = False

    def I(self, row_offset, stride=1): return str(row_offset)
    def J(self, col_offset, stride=1): return str(col_offset)
    def V(self): return str(self)

    def __str__(self): return str(self.value)

class ParameterCoeff(CoeffExpr):
    def __init__(self, value):
        self.value = value
        self.isknown = True
        self.isscalar = False
        self.is_matrix_param = True

    def to_sparse(self): return "%s = o.sparse(%s)" % (self, str(self))
    def I(self, row_offset, stride=1): return "%d + %d*%s.I" % (row_offset, stride, str(self))
    def J(self, col_offset, stride=1): return "%d + %d*%s.J" % (col_offset, stride, str(self))
    def V(self): return "%s.V" % str(self)

    def __str__(self): return self.value

class ScalarParameterCoeff(ParameterCoeff):
    def __init__(self,value):
        super(ScalarParameterCoeff, self).__init__(value)
        self.isscalar = True
        self.is_matrix_param = False

    def I(self, row_offset, stride=1): return "%d" % row_offset
    def J(self, col_offset, stride=1): return "%d" % col_offset
    def V(self): return str(self)

class NegateCoeff(CoeffExpr):
    def __init__(self, arg):
        self.arg = arg
        self.isknown = arg.isknown
        self.isscalar = arg.isscalar
        self.is_matrix_param = arg.is_matrix_param

    def to_sparse(self): return self.arg.to_sparse()
    def I(self, row_offset, stride=1): return self.arg.I(row_offset, stride)
    def J(self, col_offset, stride=1): return self.arg.J(col_offset, stride)
    def V(self): return "-(%s)" % self.arg.V()

    def __str__(self): return "-(%s)" % self.arg

class EyeCoeff(CoeffExpr):
    def __init__(self, n, coeff):
        self.n = n
        self.coeff = coeff
        self.isknown = True
        self.isscalar = False
        self.is_matrix_param = False

    def I(self, row_offset, stride=1): return "o.matrix(xrange(%d, %d, %d), (%d,1), tc='i')" % (row_offset, row_offset + stride*self.n, stride, self.n)
    def J(self, col_offset, stride=1): return "o.matrix(xrange(%d, %d, %d), (%d,1), tc='i')" % (col_offset, col_offset + stride*self.n, stride, self.n)
    def V(self): return "o.matrix(%s, (%d,1), tc='i')" % (self.coeff, self.n)

    # def __str__(self): return "_o.spmatrix(%s,range(%s),range(%s), tc='d')" % (self.coeff, self.n, self.n)

class OnesCoeff(CoeffExpr):
    def __init__(self, n, coeff, transpose = False):
        self.n = n
        self.coeff = coeff
        self.transpose = transpose
        self.isknown = True
        self.isscalar = False
	self.is_matrix_param = False

    def I(self, row_offset, stride=1):
        if self.transpose:
            return "o.matrix(%d, (%d,1), tc='i')" % (row_offset, self.n)
        else:
            return "o.matrix(xrange(%d, %d, %d), (%d,1), tc='i')" % (row_offset, row_offset + stride*self.n, stride, self.n)

    def J(self, col_offset, stride=1):
        if self.transpose:
            return "o.matrix(xrange(%d, %d, %d), (%d,1), tc='i')" % (col_offset, col_offset + stride*self.n, stride, self.n)
        else:
            return "o.matrix(%d, (%d,1), tc='i')" % (col_offset, self.n)

    def V(self): return "o.matrix(%s, (%d,1), tc='i')" % (self.coeff, self.n)

    # def __str__(self):
    #     if self.transpose:
    #         return "_o.matrix(%s,(1,%s), tc='d')" % (self.coeff, self.n)
    #     else:
    #         return "_o.matrix(%s,(%s,1), tc='d')" % (self.coeff, self.n)

class AddCoeff(CoeffExpr):
    def __init__(self, left, right):
        self.left = left
        self.right = right
        self.isknown = left.isknown and right.isknown
        self.isscalar = left.isscalar and right.isscalar
	self.is_matrix_param = left.is_matrix_param or right.is_matrix_param

    def to_sparse(self): return "result = o.sparse(%s + %s)" % (str(self.left), str(self.right))
    def I(self, row_offset, stride=1): return "%d + %d*result.I" % (row_offset, stride)
    def J(self, col_offset, stride=1): return "%d + %d*result.J" % (col_offset, stride)
    def V(self): return "result.V"

    def __str__(self): return "%s + %s" % (self.left, self.right)

class MulCoeff(CoeffExpr):
    def __init__(self, left, right):
        self.left = left
        self.right = right
        self.isknown = left.isknown and right.isknown
        self.isscalar = left.isscalar and right.isscalar
	self.is_matrix_param = left.is_matrix_param or right.is_matrix_param

    def to_sparse(self): return "result = o.sparse(%s * %s)" % (str(self.left), str(self.right))
    def I(self, row_offset, stride=1): return "%d + %d*result.I" % (row_offset, stride)
    def J(self, col_offset, stride=1): return "%d + %d*result.J" % (col_offset, stride)
    def V(self): return "result.V"

    def __str__(self): return "%s * %s" % (self.left, self.right)


class TransposeCoeff(CoeffExpr):
    def __init__(self, arg):
        self.arg = arg
        self.isknown = arg.isknown
        self.isscalar = arg.isscalar
	self.is_matrix_param = arg.is_matrix_param

    def to_sparse(self): return self.arg.to_sparse()
    def I(self, row_offset, stride=1): return self.arg.J(row_offset, stride)
    def J(self, col_offset, stride=1): return self.arg.I(col_offset, stride)
    def V(self): return self.arg.V()

    # def __str__(self): return "(%s).trans()" % self.arg

class SliceCoeff(CoeffExpr):
    # only needed for testing fixed cone sizes
    def __init__(self, arg, begin, end, transpose=False):
        self.arg = arg
        self.begin = begin
        self.end = end
        self.transpose = transpose
        self.isknown = arg.isknown
        self.isscalar = arg.isscalar
	self.is_matrix_param = arg.is_matrix_param

    # def __str__(self): return "(%s)[%s:%s]" % (self.arg, self.begin, self.end)

""" Helper functions for expr ops.

    Code structure documents the simplifications that occur.
"""
def codegen_add(x,y):
    if isinstance(x, ConstantCoeff) and isinstance(y, ConstantCoeff):
        return ConstantCoeff(x.value + y.value)
    if isinstance(x, ConstantCoeff) and x.value == 0:
        return y
    if isinstance(y,ConstantCoeff) and y.value == 0:
        return x
    if isinstance(x,EyeCoeff) and isinstance(y,EyeCoeff):
        return EyeCoeff(x.n, x.coeff + y.coeff)
    if isinstance(x,OnesCoeff) and isinstance(y,OnesCoeff) and (x.transpose == y.transpose):
        return OnesCoeff(x.n, x.coeff + y.coeff, x.transpose)
    if str(x) == str(y):
        return ConstantCoeff(2.0) * x
    return AddCoeff(x, y)

def codegen_negate(x):
    if isinstance(x,NegateCoeff):
        return x.arg
    if isinstance(x,TransposeCoeff):
        return TransposeCoeff(-x.arg)
    if isinstance(x, ConstantCoeff):
        return ConstantCoeff(-x.value)
    if isinstance(x,EyeCoeff):
        return EyeCoeff(x.n, -x.coeff)
    if isinstance(x,OnesCoeff):
        return OnesCoeff(x.n, -x.coeff, x.transpose)
    if isinstance(x,MulCoeff):
        return MulCoeff(-x.left, x.right)
    return NegateCoeff(x)

def codegen_mul(x,y):
    if isinstance(x, ConstantCoeff) and isinstance(y, ConstantCoeff):
        return ConstantCoeff(x.value * y.value)
    if isinstance(x,ConstantCoeff) and x.value == 1:
        return y
    if isinstance(y,ConstantCoeff) and y.value == 1:
        return x

    if isinstance(x,EyeCoeff) and y.isknown and y.isscalar:
        return EyeCoeff(x.n, x.coeff * y)
    if isinstance(y,EyeCoeff) and x.isknown and x.isscalar:
        return EyeCoeff(y.n, y.coeff * x)

    if isinstance(x,EyeCoeff) and isinstance(y,EyeCoeff):
        # (a*I) * (b*I) = (a*b*I)
        return EyeCoeff(x.n, x.coeff * y.coeff)
    if isinstance(x,EyeCoeff) and isinstance(x.coeff, ConstantCoeff) and x.coeff.value == 1:
        # I*x = x
        return y
    if isinstance(y,EyeCoeff) and isinstance(y.coeff, ConstantCoeff) and y.coeff.value == 1:
        # x*I = x
        return x
    if isinstance(x,EyeCoeff) and isinstance(x.coeff, ConstantCoeff) and x.coeff.value == -1:
        return -y
    if isinstance(y,EyeCoeff) and isinstance(y.coeff, ConstantCoeff) and y.coeff.value == -1:
        return -x
    if isinstance(x,OnesCoeff) and x.transpose and isinstance(y,OnesCoeff) and not y.transpose:
        # ones^T ones
        return ParameterCoeff(x.n) * x.coeff * y.coeff
    if isinstance(x,OnesCoeff) and y.isknown and y.isscalar:
        return OnesCoeff(x.n, x.coeff*y, x.transpose)
    if isinstance(y,OnesCoeff) and x.isknown and x.isscalar:
        return OnesCoeff(y.n, y.coeff*x, y.transpose)

    return MulCoeff(x, y)

def codegen_transpose(x):
    if x.isscalar:
        return x
    if isinstance(x, EyeCoeff):
        return x
    if isinstance(x, OnesCoeff):
        x.transpose = not x.transpose
        return x
    if isinstance(x,TransposeCoeff):
        return x.arg
    if isinstance(x, SliceCoeff):
        x.transpose = not x.transpose
        return x

    return TransposeCoeff(x)


def codegen_slice(x, begin, end):
    if x.isscalar:
        return x

    if isinstance(x,OnesCoeff):
        if not x.transpose:
            x.n = end - begin
        return x
    if isinstance(x,NegateCoeff):
        return NegateCoeff(x.arg.slice(begin, end))
    if isinstance(x,AddCoeff):
        return AddCoeff(x.left.slice(begin,end), x.right.slice(begin,end))
    if isinstance(x,MulCoeff):
        return MulCoeff(x.left.slice(begin,end), x.right)
    if isinstance(x,TransposeCoeff):
        return SliceCoeff(x.arg, begin, end, transpose=True)
    if isinstance(x, SliceCoeff):
        # a(2:5)(1:2)
        return SliceCoeff(x.arg, x.begin + begin, x.begin + end)

    return SliceCoeff(x, begin, end)