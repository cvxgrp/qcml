from qc_ast import Node, RelOp
from qc_vexity import Convex, Concave, Affine, Nonconvex, isaffine, isconvex, isconcave, increasing, decreasing, nonmonotone
from qc_sign import Positive, Negative, Neither, ispositive, isnegative
from qc_shape import Scalar, Vector, Matrix, isvector, ismatrix, isscalar

import scoop
import operator
import re

""" Utility functions
"""
def negate_node(x):
    """ Negates an AST node"""
    
    def _negate(x):
        """ Ensures Negate(Negate(x)) is just x."""
        if isinstance(x, Negate):
            return x.expr
        else:
            return Negate(x)
            
    if isconstant(x):
        return Constant(-x.value)
    elif isadd(x):
        if isconstant(x.left):
            return Add(Constant(-x.left.value), _negate(x.right))
        else:
            return Add(_negate(x.left), _negate(x.right))
    elif ismul(x):
        if isconstant(x.left):
            return Mul(Constant(-x.left.value), x.right)
        else:
            return Mul(_negate(x.left), x.right)
    else:
        return _negate(x)

def constant_folding(lhs,rhs,op,isop,do_op):
    """ Generic code for constant folding. Only for associative operators.
        
        op:
            Operation node (must be AST Node subclass)
        
        isop:
            Function to check if a Node is this op
            
        do_op:
            Execute the operator (e.g, for Add node, this is operator.add)
    """
    if isconstant(lhs) and isconstant(rhs):
        return Constant(do_op(lhs.value, rhs.value))
    
    left = lhs
    right = rhs
    if isconstant(lhs) and isop(rhs):
        # left is constant and right is the result of an add
        # by convention, we'll put constants on the left leaf
        if isconstant(rhs.left):
            right = rhs.right
            left = Constant(do_op(rhs.left.value,lhs.value))
        else:
            right = rhs
            left = lhs
    elif isconstant(rhs) and isop(lhs):
        # right is constant and left is the result of an add
        # by convention, we'll put constants on the right leaf
        if isconstant(lhs.left):
            right = lhs.right
            left = Constant(do_op(lhs.left.value,rhs.value))
    elif isop(lhs) and isop(rhs) and isconstant(lhs.left) and isconstant(rhs.left):
        # if adding two add nodes with constants on both sides
        left = Constant(do_op(lhs.left.value, rhs.left.value))
        right = op(lhs.right, rhs.right)
    elif isop(lhs) and isconstant(lhs.left):
        # if there are constants on the lhs, move up tree
        left = lhs.left
        right = op(lhs.right, rhs)
    elif isop(rhs) and isconstant(rhs.left):
        # if there are constants on the rhs, move up tree    
        left = rhs.left
        right = op(lhs,rhs.right)  
    
    return op(left, right)
        
def constant_folding_add(lhs,rhs):
    return constant_folding(lhs, rhs, Add, isadd, operator.add)

def constant_folding_mul(lhs,rhs):
    return constant_folding(lhs, rhs, Mul, ismul, operator.mul)
    
def distribute(lhs, rhs):
    """ Distribute multiply a*(x + y) = a*x + a*y
    """
    if isadd(lhs):
        return constant_folding_add(
            distribute(lhs.left, rhs), 
            distribute(lhs.right, rhs)
        )
    elif isadd(rhs):
        return constant_folding_add(
            distribute(lhs,rhs.left), 
            distribute(lhs,rhs.right)
        )
    elif isnegate(rhs):
        return constant_folding_mul(negate_node(lhs), rhs.expr)
    else: 
        return constant_folding_mul(lhs,rhs)

def isconstant(x):
    return isinstance(x, Constant)
    
def isadd(x):
    return isinstance(x, Add)

def ismul(x):
    return isinstance(x, Mul)

def isnegate(x):
    return isinstance(x, Negate)

def isparameter(x):
    return isinstance(x, Parameter)
    
class Expression(Node): 
    """ Expression AST node.
    
        Abstract Expression class.
    """
    def __neg__(self):
        return negate_node(self)
        
    def __sub__(self,other):
        return constant_folding_add(self, -other)
    
    def __add__(self,other):
        return constant_folding_add(self,other)
    
    def __mul__(self,other):
        return distribute(self, other)
        
    def __eq__(self, other):
        if isconstant(self) and isconstant(other):
            if self.value == other.value:
                return None
            else:
                raise TypeError("Boolean constraint %s == %s is trivially infeasible." % (self, other))
        else:
            return RelOp('==', self, other)
    
    def __le__(self, other):
        if isconstant(self) and isconstant(other):
            if self.value <= other.value:
                return None
            else:
                raise TypeError("Boolean constraint %s == %s is trivially infeasible." % (self, other))
        else:
            return RelOp('<=', self, other)
    
    def __ge__(self, other):
        if isconstant(self) and isconstant(other):
            if self.value >= other.value:
                return None
            else:
                raise TypeError("Boolean constraint %s == %s is trivially infeasible." % (self, other))
        else:
            return RelOp('>=', self, other)

class Constant(Expression):
    """ Constant AST node.
    
        Contains a floating point number. It is Affine; its sign depends on
        the sign of the float.
    """
    def __init__(self, value):
        self.value = value  # this is a float
        self.vexity = Affine()
        if float(value) >= 0.0:
            self.sign = Positive()
        else:
            self.sign = Negative()
        self.shape = Scalar()
        self.isknown = True # whether or not the expression is known at runtime
            
    def __str__(self): return str(self.value)
    
    def __repr__(self): return "Constant(%s)" % self.value
        
    def children(self): return []
    
    attr_names = ('value', 'vexity', 'sign')

class Parameter(Expression):
    """ Parameter AST node.
    
        Contains a representation of Parameters. It is Affine; its sign and
        shape are supplied from QCML.
    """
    def __init__(self, value, shape, sign):
        self.value = value
        self.vexity = Affine()
        self.sign = sign
        self.shape = shape
        self.isknown = True
    
    def __str__(self): return str(self.value)
    
    def __repr__(self): return "Parameter('%s',%s)" % (self.value, self.shape)
    
    def children(self): return []
    
    attr_names = ('value', 'vexity', 'sign','shape')
    
class Variable(Expression):
    """ Variable AST node.
    
        Contains a representation of Variables. It is Affine; its sign is 
        Neither positive nor negative. Its shape is supplied from QCML.
    """
    def __init__(self, value, shape):
        self.value = value
        self.vexity = Affine()
        self.sign = Neither()
        self.shape = shape
        self.isknown = False
        
    def __str__(self): return str(self.value)
    
    def __repr__(self): return "Variable('%s',%s)" % (self.value, self.shape)
        
    def children(self): return []
    
    attr_names = ('value', 'vexity', 'sign', 'shape')

class ToVector(Variable):
    """ ToVector AST node. Subclass of Variable.
        
        Cast a Variable with generic Shape into a vector. 
            
        Typically, the tree (whenever a Variable is used in an expression)
        looks like the following:
        
            Operations --- ToVector --- Slice --- Variable
    """
    
    def __init__(self, expr):
        self.value = expr
        self.sign = expr.sign
        self.vexity = expr.vexity
        self.isknown = expr.isknown
        if isvector(expr):
            self.shape = expr.shape
        else:
            # otherwise, construct a vector from the *first* dimension of the
            # subsequent expression (whether or not it can be done)
            #
            # we verify validity externally
            if len(expr.shape.dimensions) == 1:
                self.shape = Vector(expr.shape.dimensions[0])
            else:
                raise TypeError("Cannot construct a vector node from %s" % repr(expr))
       
    def children(self):
        nodelist = []
        if self.value is not None: nodelist.append(("expr", self.value))
        return tuple(nodelist)

    attr_names = ('vexity', 'sign', 'shape')
        
class ToMatrix(Parameter):
    """ ToMatrix AST node. Subclass of Parameter.
    
        Cast a Parameter with generic Shape into a matrix.
            
        Typically, the tree (whenever a Parameter is used in an expression)
        looks like the following:
        
            Operations --- ToMatrix --- Slice --- Parameter
        
        TODO: During rewrite stage, collapse all subclasses of Parameter into
        a single node with slice information and shape information.
    """
    
    def __init__(self, expr):
        self.value = expr
        self.sign = expr.sign
        self.vexity = expr.vexity
        self.isknown = expr.isknown
        if ismatrix(expr):
            self.shape = expr.shape
        else:
            # otherwise, construct a vector from the *first* dimension of the
            # subsequent expression (whether or not it can be done)
            #
            # we verify validity externally
            if len(expr.shape.dimensions) == 2:
                self.shape = Matrix(expr.shape.dimensions[0], expr.shape.dimensions[1])
            else:
                raise TypeError("Cannot construct a matrix node from %s" % repr(expr))
        
    def children(self):
        nodelist = []
        if self.value is not None: nodelist.append(("expr", self.value))
        return tuple(nodelist)

    attr_names = ('vexity', 'sign', 'shape')

""" Expression AST nodes
    
    What follows are nodes that are used to form expressions.
"""
class Add(Expression):
    def __init__(self, left, right):
        if isconstant(right):
            self.right = left
            self.left = right
        else:
            self.left = left
            self.right = right
        
        self.sign = left.sign + right.sign
        self.vexity = left.vexity + right.vexity
        self.shape = left.shape + right.shape
        self.isknown = left.isknown & right.isknown
    
    def __str__(self): return "%s + %s" % (self.left, self.right)

    def children(self):
        nodelist = []
        if self.left is not None: nodelist.append(("left", self.left))
        if self.right is not None: nodelist.append(("right", self.right))
        return tuple(nodelist)

    attr_names = ('vexity', 'sign','shape')

class Sum(Expression):
    def __init__(self, x):
        self.arg = x
        self.sign = x.sign
        self.vexity = x.vexity
        self.shape = Scalar()
        self.isknown = x.isknown
    
    def __str__(self): return "sum(%s)" % self.arg

    def children(self):
        nodelist = []
        if self.arg is not None: nodelist.append(("arg", self.arg))
        return tuple(nodelist)
    
    attr_names = ('vexity', 'sign','shape')
    

class Mul(Expression):
    """ Assumes the lefthand side is a Constant or a Parameter. 
    
        Effectively a unary operator.
    """
    def __init__(self, left, right):
        if isconstant(right):
            self.left = right
            self.right = left
        else:
            self.left = left
            self.right = right
            
        self.sign = left.sign * right.sign
        self.shape = left.shape * right.shape
        self.isknown = left.isknown & right.isknown
        if left.isknown:
            if isaffine(self.right):
                self.vexity = Affine()
            elif (isconvex(self.right) and ispositive(self.left)) or (isconcave(self.right) and isnegative(self.left)):
                self.vexity = Convex()
            elif (isconcave(self.right) and ispositive(self.left)) or (isconvex(self.right) and isnegative(self.left)):
                self.vexity = Concave()
            else:
                self.vexity = Nonconvex()
        else:
            # do i raise an error? do i complain about non-dcp compliance?
            self.vexity = Nonconvex()
            raise TypeError("Not DCP compliant multiply %s * %s (lefthand side should be known Constant or Parameter)" % (repr(left), repr(right)))

    # we omit parenthesis since multiply is distributed out
    def __str__(self): return "%s*%s" % (self.left, self.right)

    def children(self):
        nodelist = []
        if self.left is not None: nodelist.append(("left", self.left))
        if self.right is not None: nodelist.append(("right", self.right))
        return tuple(nodelist)

    attr_names = ('vexity', 'sign', 'shape')

class Negate(Expression):
    def __init__(self, expr):
        self.expr = expr
        self.sign = -expr.sign
        self.vexity = -expr.vexity
        self.shape = expr.shape
        self.isknown = expr.isknown
    
    # we omit the parenthesis since negate is distributed out
    def __str__(self): return "-%s" % self.expr

    def children(self):
        nodelist = []
        if self.expr is not None: nodelist.append(("expr", self.expr))
        return tuple(nodelist)

    attr_names = ('vexity', 'sign', 'shape')

class Transpose(Parameter,Expression):
    """ Can only be applied to parameters
    """
    def __init__(self, expr):
        self.value = expr
        self.sign = expr.sign
        self.vexity = expr.vexity
        self.shape = expr.shape.transpose()
        self.isknown = expr.isknown
    
    def __str__(self): return "%s'" % self.value

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
        self.isknown = False    # disallow taking functions of parameters
        # get the attributes of the atom
        try:
            self.sign, self.vexity, self.shape = scoop.atoms[self.name].attributes(*self.arglist)
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
        self.sign, self.vexity, self.shape = scoop.qc_atoms.norm(args)
        self.isknown = False
    
    def __str__(self): return "norm([%s])" % ('; '.join(map(str,self.arglist)))

    def children(self):
        nodelist = []
        if self.arglist is not None: nodelist.append(("arglist", self.arglist))
        return tuple(nodelist)
    
    attr_names = ('vexity', 'sign','shape')
    
class Abs(Expression):
    def __init__(self, x):
        self.arg = x
        self.sign, self.vexity, self.shape = scoop.qc_atoms.abs_(self.arg)
        self.isknown = False
    
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
