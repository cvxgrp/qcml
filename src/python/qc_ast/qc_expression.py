from qc_ast import RelOp
from qc_vexity import Constant, Affine, Convex, Concave, Nonconvex, \
    isaffine, isconstant, isconvex, isconcave, isnonconvex
from qc_sign import Positive, Negative, Neither, ispositive, isnegative
from qc_shape import Scalar, Vector, Matrix, isvector, ismatrix, isscalar
from ast import Node

import operator


def isnumber(x):
    return isinstance(x, Number)

def _compare(x,y,op,op_str):
    result = (x - y).simplify()
    if isnumber(result):
        if op(result.value, 0): return None
        raise ValueError("Boolean constraint %s %s %s is trivially infeasible." %(x,op_str,y))
    else:
        return RelOp(op_str, result, Number(0))

# ===============================================

class Expression(Node):
    """ Expression AST node.

        Abstract base class.
    """
    def __init__(self, vexity, shape, sign, **kwargs):
        self.vexity = vexity
        self.sign = sign
        self.shape = shape
        super(Expression, self).__init__(**kwargs)

    attr_names = ('vexity', 'sign','shape')

    def __neg__(self): return Mul(Number(-1), self)

    def __sub__(self,other): return Add(self, -other)

    def __add__(self,other): return Add(self, other)

    def __mul__(self,other): return Mul(self, other)

    def __eq__(self, other): return _compare(self, other, operator.__eq__, '==')

    def __le__(self, other): return _compare(self, other, operator.__le__, '<=')

    def __ge__(self, other): return _compare(other, self, operator.__le__, '<=')

# mixins for Leaves and Operators
class Leaf(Node):
    def __init__(self, value, **kwargs):
        self.value = value
        self.attr_names += ('value',)
        super(Leaf,self).__init__(**kwargs)

    def children(self): return []

    def __str__(self): return str(self.value)

    def simplify(self): return self

class BinaryOperator(Node):
    def __init__(self, left, right, **kwargs):
        self.left = left
        self.right = right
        self.attr_names += ()
        super(BinaryOperator, self).__init__(**kwargs)

    def children(self):
        nodelist = []
        if self.left is not None: nodelist.append(("left", self.left))
        if self.right is not None: nodelist.append(("right", self.right))
        return tuple(nodelist)

    def __str__(self): return '%s%s%s' % (self.left, self.OP_NAME, self.right)

    def __is_identity(self, expr):
        return isnumber(expr) and expr.value == self.IDENTITY

    def __is_zero(self, expr):
        return isnumber(expr) and expr.value == self.ZERO

    def _commute(self):
        # puts constants on LHS
        if isnumber(self.right):
            tmp = self.left
            self.left = self.right
            self.right = tmp

    def _associate(self):
        # commute numbers on RHS to LHS and simplify
        # tries a different association to simplify
        self._commute()
        if isinstance(self.right, self.__class__):
            # x + (y+z) = (x+y) + z
            tmp = self.right
            self.left = self.OP_FUNC(tmp.left, self.left).simplify()
            self.right = tmp.right

    def simplify(self):
        self.left = self.left.simplify()
        self.right = self.right.simplify()
        self._associate()
        if isnumber(self.left) and isnumber(self.right):
            return Number( self.OP_FUNC(self.left.value, self.right.value) )
        if self.__is_identity(self.left): return self.right
        if self.__is_identity(self.right): return self.left
        if self.__is_zero(self.left) or self.__is_zero(self.right): return Number(0)
        return self

class UnaryOperator(Node):
    def __init__(self, expr, **kwargs):
        self.expr = expr
        self.attr_names += ('expr',)
        super(UnaryOperator, self).__init__(**kwargs)

    def children(self):
        nodelist = []
        if self.expr is not None: nodelist.append(("expr", self.expr))
        return tuple(nodelist)

    def __str__(self):
        if self.IS_POSTFIX:
            return '%s%s' % (self.expr, self.OP_NAME)
        return '%s%s' % (self.OP_NAME, self.expr)

    def simplify(self):
        self.expr = self.expr.simplify()
        if isinstance(self.expr, Number):
            result = self.OP_FUNC(self.expr.value)
            return Number(result)
        return self


class Number(Expression, Leaf):
    """ Number AST node.

        Contains a floating point number. It is Affine; its sign depends on
        the sign of the float.
    """
    def __init__(self, value):
        if float(value) >= 0.0: sign = Positive()
        else: sign = Negative()
        super(Number, self).__init__(value = value, vexity = Constant(), shape = Scalar(), sign = sign)

    def __repr__(self): return "Number(%s)" % self.value

class Parameter(Expression, Leaf):
    """ Parameter AST node.

        Contains a representation of Parameters. It is Affine; its sign and
        shape are supplied from QCML.
    """
    def __init__(self, value, shape, sign):
        super(Parameter, self).__init__(value = value, vexity = Constant(), shape = shape, sign = sign)

    def __repr__(self): return "Parameter('%s',%s)" % (self.value, self.shape)

class Variable(Expression, Leaf):
    """ Variable AST node.

        Contains a representation of Variables. It is Affine; its sign is
        Neither positive nor negative. Its shape is supplied from QCML.
    """
    def __init__(self, value, shape):
        super(Variable, self).__init__(value = value, vexity = Affine(), shape = shape, sign = Neither())

    def __repr__(self): return "Variable('%s',%s)" % (self.value, self.shape)


from qc_operator import Add, Mul