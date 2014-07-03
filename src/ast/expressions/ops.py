from ... properties import sign, shape, curvature
import expression as e

import operator
# import qcml

def is_constant_mul(expr):
    return isinstance(expr, Mul) and e.isnumber(expr.left)

# multiplication taking sign into account
def _signed_multiply(left, right):
    if curvature.isconstant(left):
        if curvature.isconstant(right):
            return curvature.Constant()
        if curvature.isaffine(right):
            return curvature.Affine()
        if curvature.isconvex(right) and sign.ispositive(left):
            return curvature.Convex()
        if curvature.isconcave(right) and sign.isnegative(left):
            return curvature.Convex()
        if curvature.isconcave(right) and sign.ispositive(left):
            return curvature.Concave()
        if curvature.isconvex(right) and sign.isnegative(left):
            return curvature.Concave()
        return curvature.Nonconvex()
    else:
        # do i raise an error? do i complain about non-dcp compliance?
        #curvature = Nonconvex()
        raise TypeError("Not DCP compliant multiply %s * %s (lefthand side should be known Number or Parameter)" % (repr(left), repr(right)))

""" Expression AST nodes

    What follows are nodes that are used to form expressions.
"""
class Add(e.Expression, e.BinaryOperatorMixin):
    OP_NAME = ' + '
    OP_FUNC = operator.__add__
    IDENTITY = 0
    ZERO = False

    def __init__(self, left, right):
        setup = {
            'left': left,
            'right': right,
            'curvature': left.curvature + right.curvature,
            'sign': left.sign + right.sign,
            'shape': left.shape + right.shape
        }
        super(Add, self).__init__(**setup)

    def _associate(self):
        super(Add, self)._associate()
        # this additional check shows up since Add is fully commutative,
        # unlike Mul (i.e., matrix multiply is not commutative)
        if isinstance(self.right, Add):
            # x + (y+z) = (x+z) + y
            tmp = self.right
            self.left = (tmp.right + self.left).simplify()
            self.right = tmp.left

    def collect(self):
        # collect x + n*x into (n+1)*x
        if str(self.left) == str(self.right):
            return Mul(e.Number(2), self.left).simplify()
        if is_constant_mul(self.right) and str(self.left) == str(self.right.right):
            return ( (self.right.left + e.Number(1)) * self.left ).simplify()
        if is_constant_mul(self.left) and str(self.right) == str(self.left.right):
            return ( (self.left.left + e.Number(1)) * self.right ).simplify()
        return self

class Mul(e.Expression, e.BinaryOperatorMixin):
    """ Assumes the lefthand side is a Number or a Parameter.

        Effectively a unary operator.
    """
    OP_NAME = '*'
    OP_FUNC = operator.__mul__
    IDENTITY = 1
    ZERO = 0

    def __init__(self, left, right):
        setup = {
            'left': left,
            'right': right,
            'curvature': _signed_multiply(left, right),
            'sign': left.sign * right.sign,
            'shape': left.shape * right.shape
        }

        super(Mul, self).__init__(**setup)

    def distribute(self):
        if isinstance(self.right, Add):
            # a*(x + y) = a*x + b*y
            tmp = self.right
            return (self.left*tmp.left + self.left*tmp.right).simplify()
        if isinstance(self.left, Add):
            # (a + b)*x = a*x + b*x
            tmp = self.left
            return (tmp.left*self.right + tmp.right*self.right).simplify()
        return self


class Sum(e.Expression, e.UnaryOperatorMixin):

    def sum_func(self,x):
        if isinstance(x, e.Expression): return Sum(x)
        return x

    OP_NAME = "1'*"
    IS_POSTFIX = False
    OP_FUNC = sum_func

    def __init__(self, x):
        super(Sum, self).__init__(expr = x, sign = x.sign, curvature = x.curvature, shape = shape.Scalar())

    def distribute(self):
        if isinstance(self.expr, Add):
            return (Sum(self.expr.left) + Sum(self.expr.right)).simplify()
        if isinstance(self.expr, Mul) and shape.isscalar(self.expr.left):
            return (Mul(self.expr.left, Sum(self.expr.right))).simplify()
        return self

class Transpose(e.Expression, e.UnaryOperatorMixin):
    """ Can only be applied to parameters
    """
    def transpose(self,x):
        if isinstance(x, e.Expression): return Transpose(x)
        return x

    OP_NAME = "'"
    IS_POSTFIX = True
    OP_FUNC = transpose

    def __init__(self, expr):
        super(Transpose, self).__init__(expr = expr, shape = expr.shape.transpose(), curvature = expr.curvature, sign = expr.sign)

    def distribute(self):
        if shape.isscalar(self.expr):
            return self.expr
        if isinstance(self.expr, Add):
            return (Transpose(self.expr.left) + Transpose(self.expr.right)).simplify()
        if isinstance(self.expr, Mul):
            return (Transpose(self.expr.right) * Transpose(self.expr.left)).simplify()
        return self

# Code below for slicing is outdated. Needs to be rethought

class Slice(e.Expression, e.UnaryOperatorMixin):
    """ Can only be applied to parameters or variables.

        At the moment, assumes that begin and end are of type int
    """
    def slice(self,x):
        if isinstance(x, e.Expression): return Slice(x)
        return x

    OP_NAME = "'"
    IS_POSTFIX = True
    OP_FUNC = slice

    def __init__(self, expr, begin, end, dim):
        assert (type(begin) is int), "Expected beginning index to be an integer"
        assert (type(end) is int), "Expected end index to be an integer"
        assert (begin < end), "Beginning slice should be less than end"

        super(Slice, self).__init__(expr, expr.slice(begin, end, dim), expr.sign)

        self.slice_dim = dim
        self.begin = begin
        self.end = end

    def __str__(self):
        if isscalar(self.value):
            return "%s" % self.value
        if isvector(self.value):
            return "%s(%s:%s)" % (self.value, self.begin, self.end)

        dims = self.value.shape.num_dimensions*[':']
        dims[self.slice_dim] = "%s:%s" % (self.begin, self.end)
        return "%s(%s)" % (self.value, ','.join(dims))

    def children(self):
        nodelist = []
        if self.value is not None: nodelist.append(("value", self.value))
        return tuple(nodelist)

    attr_names = ('curvature', 'sign', 'shape')

# class Norm(e.Expression):
#     def __init__(self, args):
#         self.arglist = args
#         self.sign, self.curvature, self.shape = qcml.qc_atoms.norm(args)
#
#     def __str__(self): return "norm(%s)" % (', '.join(map(str,self.arglist)))
#
#     def children(self):
#         nodelist = []
#         if self.arglist is not None: nodelist.append(("arglist", self.arglist))
#         return tuple(nodelist)
#
#     attr_names = ('curvature', 'sign','shape')
#
# class Abs(e.Expression):
#     def __init__(self, x):
#         self.arg = x
#         self.sign, self.curvature, self.shape = qcml.qc_atoms.abs_(self.arg)
#
#     def __str__(self): return "abs(%s)" % self.arg
#
#     def children(self):
#         nodelist = []
#         if self.arg is not None: nodelist.append(("arg", self.arg))
#         return tuple(nodelist)
#
#     attr_names = ('curvature', 'sign','shape')

class Vstack(e.Expression):
    """ Vstack AST node.

        Forms the vertical concatenation: [x; y; z].
    """
    def __init__(self, args):
        self.arglist = args
        self.curvature = sum(map(lambda x: x.curvature, args))    # WRONG
        self.sign = sum(map(lambda x: x.sign, args))        # WRONG
        self.shape = shape.Scalar() #stack(map(lambda x: x.shape, args))

    def __str__(self): return "[%s]" % ('; '.join(map(str, self.arglist)))

    def children(self):
        nodelist = []
        if self.arglist is not None: nodelist.append(("arglist", self.arglist))
        return tuple(nodelist)

    attr_names = ('curvature', 'sign', 'shape')

# class ToVector(Expression, UnaryOperator):
#     """ ToVector AST node. Subclass of Variable.
#
#         Cast a Variable with generic Shape into a vector.
#
#         Typically, the tree (whenever a Variable is used in an expression)
#         looks like the following:
#
#             Operations --- ToVector --- Slice --- Variable
#     """
#     OP_NAME = ""
#     IS_POSTFIX = False
#
#     def __init__(self, expr):
#         if isvector(expr):
#             super(ToVector, self).__init__(expr = expr, curvature = expr.curvature, shape = expr.shape, sign = expr.sign)
#         else:
#             raise TypeError("Cannot construct a vector node from %s" % repr(expr))
#
# class ToMatrix(Expression, UnaryOperator):
#     """ ToMatrix AST node. Subclass of Parameter.
#
#         Cast a Parameter with generic Shape into a matrix.
#
#         Typically, the tree (whenever a Parameter is used in an expression)
#         looks like the following:
#
#             Operations --- ToMatrix --- Slice --- Parameter
#
#         TODO: During rewrite stage, collapse all subclasses of Parameter into
#         a single node with slice information and shape information.
#     """
#     OP_NAME = ""
#     IS_POSTFIX = False
#
#     def __init__(self, expr):
#         if ismatrix(expr):
#             super(ToMatrix, self).__init__(expr = expr, curvature = expr.curvature, shape = expr.shape, sign = expr.sign)
#         else:
#             raise TypeError("Cannot construct a matrix node from %s" % repr(expr))

