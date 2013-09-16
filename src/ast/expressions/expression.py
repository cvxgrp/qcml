from .. node import Node
from .. constraints import LinearEquality, LinearInequality
import operator
from abc import ABCMeta


def isnumber(x):
    return isinstance(x, Number)

def _compare(x,y,op,op_str):
    result = (x - y).simplify()
    if isnumber(result):
        if op(result.value, 0): return None
        raise ValueError("Boolean constraint %s %s %s is trivially infeasible." %(x,op_str,y))
    else:
        if op_str == '==':
            return LinearEquality(x, y)
        if op_str == '<=':
            # at this stage, all >= have been swapped into <=
            return LinearInequality(x, y)
        raise ValueError("Attempted to construct unknown boolean constraint %s." % (op_str))

# ===============================================

class Expression(Node):
    __metaclass__ = ABCMeta
    """ Expression AST node.

        Abstract base class.
    """
    def __init__(self, curvature, shape, sign, **kwargs):
        self.curvature = curvature
        self.sign = sign
        self.shape = shape
        super(Expression, self).__init__(**kwargs)

    def __neg__(self): return Mul(Number(-1), self)

    def __sub__(self,other): return Add(self, -other)

    def __add__(self,other): return Add(self, other)

    def __mul__(self,other): return Mul(self, other)

    def __eq__(self, other): return _compare(self, other, operator.__eq__, '==')

    def __le__(self, other): return _compare(self, other, operator.__le__, '<=')

    def __ge__(self, other): return _compare(other, self, operator.__le__, '<=')

    def info(self):
        return "%s: %s, %s, %s" % (self.__class__.__name__, self.curvature, self.sign, self.shape)


class LeafMixin(Node):
    def __init__(self, value, **kwargs):
        self.value = value
        super(LeafMixin,self).__init__(**kwargs)

    def children(self): return []

    def __str__(self): return str(self.value)

    def simplify(self): return self

    def canonicalize(self): return (self, [])

    def info(self):
        return "%s: %s, %s, %s, %s" % (self.__class__.__name__, self.curvature, self.sign, self.shape, self.value)


class BinaryOperatorMixin(Node):
    def __init__(self, left, right, **kwargs):
        self.left = left
        self.right = right
        super(BinaryOperatorMixin, self).__init__(**kwargs)

    def children(self):
        if self.left is not None: yield self.left
        if self.right is not None: yield self.right

    def __str__(self): return '%s%s%s' % (self.left, self.OP_NAME, self.right)

    def __is_identity(self, expr):
        return isnumber(expr) and expr.value == self.IDENTITY

    def __is_zero(self, expr):
        return isnumber(expr) and expr.value == self.ZERO

    def _commute_number(self):
        # puts number on LHS
        if isnumber(self.right):
            self.left, self.right = self.right, self.left

    def _associate(self):
        # commute numbers on RHS to LHS and simplify
        # tries a different association to simplify
        self._commute_number()
        if isinstance(self.right, self.__class__):
            # x op (y op z) = (x op y) op z
            tmp = self.right
            self.left = self.OP_FUNC(self.left, tmp.left).simplify()
            self.right = tmp.right
        if isinstance(self.left, self.__class__):
            # (x op y) op z = x op (y op z)
            tmp = self.left
            self.left = tmp.left
            self.right = self.OP_FUNC(tmp.right, self.right).simplify()

    def distribute_or_collect(self):
        if hasattr(self, 'distribute'): return self.distribute()
        if hasattr(self, 'collect'): return self.collect()
        return self

    def simplify(self):
        self.left = self.left.simplify()
        self.right = self.right.simplify()

        self._associate()
        if isnumber(self.left) and isnumber(self.right):
            return Number( self.OP_FUNC(self.left.value, self.right.value) )
        if self.__is_identity(self.left): return self.right
        if self.__is_identity(self.right): return self.left
        if self.__is_zero(self.left) or self.__is_zero(self.right): return Number(0)
        return self.distribute_or_collect()

    def canonicalize(self):
        lh_obj, lh_constraints = self.left.canonicalize()
        rh_obj, rh_constraints = self.right.canonicalize()
        obj = self.OP_FUNC(lh_obj, rh_obj)
        return (obj, lh_constraints + rh_constraints)

class UnaryOperatorMixin(Node):
    def __init__(self, expr, **kwargs):
        self.expr = expr
        super(UnaryOperatorMixin, self).__init__(**kwargs)

    def children(self):
        if self.expr is not None: yield self.expr

    def __str__(self):
        if self.IS_POSTFIX:
            return '%s%s' % (self.expr, self.OP_NAME)
        return '%s%s' % (self.OP_NAME, self.expr)

    def distribute(self): return self

    def simplify(self):
        self.expr = self.expr.simplify()
        if isnumber(self.expr):
            return Number( self.OP_FUNC(self.expr.value) )
        return self.distribute()

    def canonicalize(self):
        obj, constraints = self.expr.canonicalize()
        obj = self.OP_FUNC(obj)
        return (obj, constraints)

from . leaf import Number, Parameter
from . variable import Variable
from ops import *
