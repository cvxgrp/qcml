#from qc_ast import RelOp
from qc_vexity import Constant, Affine, Convex, Concave, Nonconvex, \
    isaffine, isconstant, isconvex, isconcave, isnonconvex
from qc_sign import Positive, Negative, Neither, ispositive, isnegative
from qc_shape import Scalar, Vector, Matrix, isvector, ismatrix, isscalar
from qc_expression import Expression, UnaryOperator, BinaryOperator, Leaf, Number, Parameter, Variable, isnumber

import operator
import qcml

def is_constant_mul(expr):
    return isinstance(expr, Mul) and isinstance(expr.left, Number)

# multiplication taking sign into account
def _signed_multiply(left, right):
    if isconstant(left):
        if isconstant(right): return Constant()
        if isaffine(right): return Affine()
        if isconvex(right) and ispositive(left): return Convex()
        if isconcave(right) and isnegative(left): return Convex()
        if isconcave(right) and ispositive(left): return Concave()
        if isconvex(right) and isnegative(left): return Concave()
        return Nonconvex()
    else:
        # do i raise an error? do i complain about non-dcp compliance?
        #vexity = Nonconvex()
        raise TypeError("Not DCP compliant multiply %s * %s (lefthand side should be known Number or Parameter)" % (repr(left), repr(right)))

""" Expression AST nodes

    What follows are nodes that are used to form expressions.
"""
class Add(Expression, BinaryOperator):
    OP_NAME = ' + '
    OP_FUNC = operator.__add__
    IDENTITY = 0
    ZERO = False

    def __init__(self, left, right):
        setup = {
            'left': left,
            'right': right,
            'vexity': left.vexity + right.vexity,
            'sign': left.sign + right.sign,
            'shape': left.shape + right.shape
        }
        super(Add, self).__init__(**setup)

    def _associate(self):
        super(Add, self)._associate()
        # these additional checks show up since Add is fully associative,
        # unlike Mul (i.e., matrix multiply is not associative)
        if isinstance(self.left, Add):
            # (x+y) + z = x + (y+z)
            tmp = self.left
            self.left = tmp.left
            self.right = (tmp.right + self.right).simplify()
        if isinstance(self.right, Add):
            # x + (y+z) = (x+z) + y
            tmp = self.right
            self.left = (tmp.right + self.left).simplify()
            self.right = tmp.left

    def collect(self):
        if str(self.left) == str(self.right):
            return Mul(Number(2), self.left).simplify()
        # collect x + n*x into (n+1)*x
        if is_constant_mul(self.right) and str(self.left) == str(self.right.right):
            return ( (self.right.left + Number(1)) * self.left ).simplify()

        if is_constant_mul(self.left) and str(self.right) == str(self.left.right):
            return ( (self.left.left + Number(1)) * self.right ).simplify()

        return self

    def simplify(self):
        result = super(Add, self).simplify()
        if isinstance(result, Add): return result.collect()
        return result

class Sum(Expression, UnaryOperator):
    OP_NAME = 'sum'
    IS_POSTFIX = False
    OP_FUNC = sum

    def __init__(self, x):
        super(Sum, self).__init__(expr = x, sign = x.sign, vexity = x.vexity, shape = Scalar())


class Mul(Expression, BinaryOperator):
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
            'vexity': _signed_multiply(left, right),
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


    def simplify(self):
        result = super(Mul, self).simplify()
        # distribute
        if isinstance(result, Mul): return result.distribute()
        return result

# ... TODO: up to here, the code is done...

class Transpose(Expression, UnaryOperator):
    """ Can only be applied to parameters
    """
    OP_NAME = "'"
    IS_POSTFIX = True
    OP_FUNC = lambda x: x

    def __init__(self, expr):
        super(Transpose, self).__init__(expr = expr, shape = expr.shape.transpose(), vexity = expr.vexity, sign = expr.sign)

class Slice(Parameter,Variable):
    """ Can only be applied to parameters or variables.

        At the moment, assumes that begin and end are of type int
    """
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

    attr_names = ('vexity', 'sign', 'shape')

class Atom(Expression):
    """ Atom AST node.

        Stores the name of the atom and its arguments
    """
    def __init__(self,name,arguments):
        self.name = name
        self.arglist = arguments
        # get the attributes of the atom
        try:
            self.sign, self.vexity, self.shape = qcml.atoms[self.name].attributes(*self.arglist)
        except TypeError as e:
            msg = re.sub(r'attributes\(\)', r'%s' % self.name, str(e))
            raise TypeError(msg)

    def __str__(self): return "%s(%s)" % (self.name, ','.join(map(str, self.arglist)))

    def children(self):
        nodelist = []
        if self.arglist is not None: nodelist.append(("arglist", self.arglist))
        return tuple(nodelist)

    attr_names = ('name', 'vexity', 'sign', 'shape')

class Norm(Expression):
    def __init__(self, args):
        self.arglist = args
        self.sign, self.vexity, self.shape = qcml.qc_atoms.norm(args)

    def __str__(self): return "norm(%s)" % (', '.join(map(str,self.arglist)))

    def children(self):
        nodelist = []
        if self.arglist is not None: nodelist.append(("arglist", self.arglist))
        return tuple(nodelist)

    attr_names = ('vexity', 'sign','shape')

class Abs(Expression):
    def __init__(self, x):
        self.arg = x
        self.sign, self.vexity, self.shape = qcml.qc_atoms.abs_(self.arg)

    def __str__(self): return "abs(%s)" % self.arg

    def children(self):
        nodelist = []
        if self.arg is not None: nodelist.append(("arg", self.arg))
        return tuple(nodelist)

    attr_names = ('vexity', 'sign','shape')

class Vstack(Expression):
    """ Vstack AST node.

        Forms the vertical concatenation: [x; y; z].
    """
    def __init__(self, args):
        self.arglist = args
        self.vexity = sum(map(lambda x: x.vexity, args))    # WRONG
        self.sign = sum(map(lambda x: x.sign, args))        # WRONG
        self.shape = Scalar() #stack(map(lambda x: x.shape, args))

    def __str__(self): return "[%s]" % ('; '.join(map(str, self.arglist)))

    def children(self):
        nodelist = []
        if self.arglist is not None: nodelist.append(("arglist", self.arglist))
        return tuple(nodelist)

    attr_names = ('vexity', 'sign', 'shape')

class ToVector(Expression, UnaryOperator):
    """ ToVector AST node. Subclass of Variable.

        Cast a Variable with generic Shape into a vector.

        Typically, the tree (whenever a Variable is used in an expression)
        looks like the following:

            Operations --- ToVector --- Slice --- Variable
    """
    OP_NAME = ""
    IS_POSTFIX = False

    def __init__(self, expr):
        if isvector(expr):
            super(ToVector, self).__init__(expr = expr, vexity = expr.vexity, shape = expr.shape, sign = expr.sign)
        else:
            raise TypeError("Cannot construct a vector node from %s" % repr(expr))

class ToMatrix(Expression, UnaryOperator):
    """ ToMatrix AST node. Subclass of Parameter.

        Cast a Parameter with generic Shape into a matrix.

        Typically, the tree (whenever a Parameter is used in an expression)
        looks like the following:

            Operations --- ToMatrix --- Slice --- Parameter

        TODO: During rewrite stage, collapse all subclasses of Parameter into
        a single node with slice information and shape information.
    """
    OP_NAME = ""
    IS_POSTFIX = False

    def __init__(self, expr):
        if ismatrix(expr):
            super(ToMatrix, self).__init__(expr = expr, vexity = expr.vexity, shape = expr.shape, sign = expr.sign)
        else:
            raise TypeError("Cannot construct a matrix node from %s" % repr(expr))

