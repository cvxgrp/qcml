# TODO: define code generation objects
""" CoeffExpr object.
    Used to construct coefficients of affine expressions.

    Allows us to create abstract objects to represent OnesCoeff, EyeCoeff, etc. and
    perform basic addition, multiplication, etc. on them. Assumes that the
    coefficient math has already passed DCP checking.

    TODO: AddCoeff test cases to CoeffExpr
"""
from .. import code
class CoeffExpr(code.Code):
    # operations only occur on objects with the same shape
    def __add__(self, other): return codegen_add(self, other)

    def __sub__(self, other): return self + (-other)

    def __neg__(self): return codegen_negate(self)

    def __mul__(self,other): return codegen_mul(self, other)

    def trans(self): return codegen_transpose(self)

    # only used during code generation
    # for vertical slicing
    def slice(self, begin, end): return codegen_slice(self, begin, end)

    def nnz(self): return ""
    def to_sparse(self): return ""
    def I(self, row_offset, stride=1): return ""
    def J(self, col_offset, stride=1): return ""
    def V(self): return ""

class ConstantCoeff(CoeffExpr):
    def __init__(self, value):
        self.value = value
        self.isknown = True
        self.isscalar = True
        self.is_matrix_param = False

    def nnz(self): return "1"
    def I(self, row_offset, stride=1): return code.Just(row_offset)
    def J(self, col_offset, stride=1): return code.Just(col_offset)
    def V(self): return code.Just(self)

class ParameterCoeff(CoeffExpr):
    def __init__(self, value, shape):
        self.value = value
        self.isknown = True
        self.isscalar = False
        self.is_matrix_param = True
        self.rows, self.cols = shape

    def nnz(self): return code.NNZ(self)
    def to_sparse(self): return code.Assign(self, self)
    def I(self, row_offset, stride=1): return code.LoopRows(self, row_offset, stride)
    def J(self, col_offset, stride=1): return code.LoopCols(self, col_offset, stride)
    def V(self): return code.LoopOver(self)


class ScalarParameterCoeff(ParameterCoeff):
    def __init__(self,value):
        super(ScalarParameterCoeff, self).__init__(value, (1,1))
        self.isscalar = True
        self.is_matrix_param = False

    def nnz(self): return "1"
    def to_sparse(self): return ""
    def I(self, row_offset, stride=1): return code.Just(row_offset)
    def J(self, col_offset, stride=1): return code.Just(col_offset)
    def V(self): return code.Just(self)

class NegateCoeff(CoeffExpr):
    def __init__(self, arg):
        self.arg = arg
        self.isknown = arg.isknown
        self.isscalar = arg.isscalar
        self.is_matrix_param = arg.is_matrix_param

    def nnz(self): return self.arg.nnz()
    def to_sparse(self): return self.arg.to_sparse()
    def I(self, row_offset, stride=1): return self.arg.I(row_offset, stride)
    def J(self, col_offset, stride=1): return self.arg.J(col_offset, stride)
    def V(self):
        if self.arg.isscalar:
            return code.Just(self)
        else:
            return code.LoopOver(self.arg.V(),"-%s")


class EyeCoeff(CoeffExpr):
    def __init__(self, n, coeff):
        self.n = n
        self.coeff = coeff
        self.isknown = True
        self.isscalar = False
        self.is_matrix_param = False

    def nnz(self): return self.n
    def I(self, row_offset, stride=1): return code.Range(row_offset, row_offset + stride*self.n, stride)
    def J(self, col_offset, stride=1): return code.Range(col_offset, col_offset + stride*self.n, stride)
    def V(self): return code.Repeat(self.coeff, self.n)

    # def __str__(self): return "_o.spmatrix(%s,range(%s),range(%s), tc='d')" % (self.coeff, self.n, self.n)

class OnesCoeff(CoeffExpr):
    def __init__(self, n, coeff, transpose = False):
        self.n = n
        self.coeff = coeff
        self.transpose = transpose
        self.isknown = True
        self.isscalar = False
        if transpose:
            self.is_matrix_param = True
        else:
            self.is_matrix_param = False

    def nnz(self): return self.n

    def I(self, row_offset, stride=1):
        if self.transpose:
            return code.Repeat(row_offset, self.n)
        else:
            return code.Range(row_offset, row_offset + stride*self.n, stride)

    def J(self, col_offset, stride=1):
        if self.transpose:
            return code.Range(col_offset, col_offset + stride*self.n, stride)
        else:
            return code.Repeat(col_offset, self.n)

    def V(self): return code.Repeat(self.coeff, self.n)

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

    def nnz(self): return code.NNZ("result")
    def to_sparse(self): return code.Assign("result", self)
    def I(self, row_offset, stride=1): return code.LoopRows("result", row_offset, stride)
    def J(self, col_offset, stride=1): return code.LoopCols("result", col_offset, stride)
    def V(self): return code.LoopOver("result")

class MulCoeff(CoeffExpr):
    def __init__(self, left, right):
        self.left = left
        self.right = right
        self.isknown = left.isknown and right.isknown
        self.isscalar = left.isscalar and right.isscalar
        self.is_matrix_param = left.is_matrix_param or right.is_matrix_param

    def nnz(self):
        if self.left.isscalar:
            return self.right.nnz()
        else:
            return code.NNZ("result")
    def to_sparse(self):
        if self.left.isscalar:
            return code.Assign(self.right, self.right)
        else:
            return code.Assign("result", self)
    def I(self, row_offset, stride=1):
        if self.left.isscalar:
            return code.LoopRows(self.right, row_offset, stride)
        else:
            return code.LoopRows("result", row_offset, stride)
    def J(self, col_offset, stride=1):
        if self.left.isscalar:
            return code.LoopCols(self.right, col_offset, stride)
        else:
            return code.LoopCols("result", col_offset, stride)
    def V(self):
        if self.left.isscalar:
            return code.LoopOver(self.right, "{0}*%s".format(self.left.value))
        else:
            return code.LoopOver("result")


class TransposeCoeff(CoeffExpr):
    def __init__(self, arg):
        self.arg = arg
        self.isknown = arg.isknown
        self.isscalar = arg.isscalar
        self.is_matrix_param = arg.is_matrix_param

    def nnz(self): return self.arg.nnz()
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
    if isinstance(x,ConstantCoeff) and x.value == -1:
        return -y
    if isinstance(y,ConstantCoeff) and y.value == -1:
        return -x

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
        # -I*x = -x
        return -y
    if isinstance(y,EyeCoeff) and isinstance(y.coeff, ConstantCoeff) and y.coeff.value == -1:
        # x*-I = -x
        return -x
    if isinstance(x,OnesCoeff) and x.transpose and isinstance(y,OnesCoeff) and not y.transpose:
        # ones^T ones
        return ParameterCoeff(x.n) * x.coeff * y.coeff
    if isinstance(x,OnesCoeff) and y.isknown and y.isscalar:
        return OnesCoeff(x.n, x.coeff*y, x.transpose)
    if isinstance(y,OnesCoeff) and x.isknown and x.isscalar:
        return OnesCoeff(y.n, y.coeff*x, y.transpose)

    if isinstance(x, OnesCoeff) and isinstance(y, EyeCoeff) and x.transpose:
        # ones^T eye
        return OnesCoeff(x.n, y.coeff * x.coeff, x.transpose)

    return MulCoeff(x, y)

def codegen_transpose(x):
    if x.isscalar:
        return x
    if isinstance(x, EyeCoeff):
        return x
    if isinstance(x, OnesCoeff):
        x.transpose = not x.transpose
        if x.transpose:
            x.is_matrix_param = True
        else:
            x.is_matrix_param = False
        return x
    if isinstance(x,TransposeCoeff):
        return x.arg
    if isinstance(x, SliceCoeff):
        x.transpose = not x.transpose
        return x
    if isinstance(x,MulCoeff) and x.left.isscalar and isinstance(x.right, TransposeCoeff):
        return MulCoeff(x.left, x.right.arg)

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
