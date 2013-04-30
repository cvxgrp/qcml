# Built from PyCParser's AST and Python's AST

from qc_vexity import Convex, Concave, Affine, Nonconvex, isaffine, isconvex, isconcave
from qc_sign import Positive, Negative, Neither, ispositive, isnegative
from qc_shape import Scalar, Vector, Matrix, isvector, ismatrix, isscalar
#from utils import negate_node, constant_folding_add, distribute

import scoop
import sys, re
import operator

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
    else: 
        return constant_folding_mul(lhs,rhs)

def isconstant(x):
    return isinstance(x, Constant)
    
def isadd(x):
    return isinstance(x, Add)

def ismul(x):
    return isinstance(x, Mul)

def isparameter(x):
    return isinstance(x, Parameter)
    
""" Actual AST class
"""

class Node(object):
    """ Abstract base class for AST nodes.
    """
    def children(self):
        """ A sequence of all children that are Nodes
        """
        pass
    
    def show(self, buf=sys.stdout, offset=0, attrnames=False, nodenames=False, _my_node_name=None):
        """ Pretty print the Node and all its attributes and
            children (recursively) to a buffer.
            
            buf:   
                Open IO buffer into which the Node is printed.
            
            offset: 
                Initial offset (amount of leading spaces) 
            
            attrnames:
                True if you want to see the attribute names in
                name=value pairs. False to only see the values.
                
            nodenames:
                True if you want to see the actual node names 
                within their parents.
            
            showcoord:
                Do you want the coordinates of each Node to be
                displayed.
        """
        lead = ' ' * offset
        if nodenames and _my_node_name is not None:
            buf.write(lead + self.__class__.__name__+ ' <' + _my_node_name + '>: ')
        else:
            buf.write(lead + self.__class__.__name__+ ': ')

        if self.attr_names:
            if attrnames:
                nvlist = [(n, getattr(self,n)) for n in self.attr_names]
                attrstr = ', '.join('%s=%s' % nv for nv in nvlist)
            else:
                vlist = [getattr(self, n) for n in self.attr_names]
                attrstr = ', '.join('%s' % v for v in vlist)
            buf.write(attrstr)

        # if showcoord:
        #     buf.write(' (at %s)' % self.coord)
        buf.write('\n')

        for (child_name, child) in self.children():
            child.show(
                buf,
                offset=offset + 2,
                attrnames=attrnames,
                nodenames=nodenames,
                _my_node_name=child_name)

class NodeTransformer(object):
    """ A base NodeTransformer class for visiting c_ast nodes. 
        Subclass it and define your own visit_XXX methods, where
        XXX is the class name you want to visit with these 
        methods.
        
        For example:
        
        class ConstantVisitor(NodeTransformer):
            def __init__(self):
                self.values = []
            
            def visit_Constant(self, node):
                self.values.append(node.value)

        Creates a list of values of all the constant nodes 
        encountered below the given node. To use it:
        
        cv = ConstantVisitor()
        cv.visit(node)
        
        Notes:
        
        *   generic_visit() will be called for AST nodes for which 
            no visit_XXX method was defined. 
        *   The children of nodes for which a visit_XXX was 
            defined will not be visited - if you need this, call
            generic_visit() on the node. 
            You can use:
                NodeTransformer.generic_visit(self, node)
        *   Modeled after Python's own AST visiting facilities
            (the ast module of Python 3.0)
    """
    def visit(self, node):
        """ Visit a node. 
        """
        method = 'visit_' + node.__class__.__name__
        visitor = getattr(self, method, self.generic_visit)
        return visitor(node)
        
    def generic_visit(self, node):
        """ Called if no explicit visitor function exists for a 
            node. Implements preorder visiting of the node.
        """
        for c_name, c in node.children():
            self.visit(c)
            

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
            return self.value == other.value
        else:
            return RelOp('==', self, other)
    
    def __le__(self, other):
        if isconstant(self) and isconstant(other):
            return self.value <= other.value
        else:
            return RelOp('<=', self, other)
    
    def __ge__(self, other):
        if isconstant(self) and isconstant(other):
            return self.value >= other.value
        else:
            return RelOp('>=', self, other)
    
class Program(Node):
    """ Program AST node.
    
        This node is root node for the program. Its children is a *single*
        Objective node and a list of RelOp nodes.
        
        It is DCP compliant when the Objective is DCP and the RelOp's are DCP.
    """
    def __init__(self, objective, constraints, variables, parameters={}, dimensions=set()):
        self.objective = objective
        self.constraints = constraints
        self.is_dcp = objective.is_dcp and all(c.is_dcp for c in constraints)
        self.variables = variables      # variables declared by the user
        self.parameters = parameters    # parameters declared by the user
        self.dimensions = dimensions    # dimensions declared by the user
        self.new_variables = {}         # new variables introduced by rewriting
    
    def __str__(self): return "%s\nsubject to\n    %s" % (self.objective, '\n    '.join(map(str,self.constraints)))
        
    def children(self):
        nodelist = []
        if self.objective is not None: nodelist.append(("objective", self.objective))
        if self.constraints is not None: 
            for c in self.constraints:
                nodelist.append(("constraint" , c))
        return tuple(nodelist)
    
    def add_constraint(self, c):
        """ Allows us to add constraints to the program
        """
        self.constraints.append(c)
    
    attr_names = ('is_dcp',)

class Objective(Node):
    """ Objective AST node.
    
        This node contains the representation of the objective. It also stores
        the objective's sense (minimize, maximize, or find).
        
        It is DCP compliant when the objective's expression agrees with its
        sense. Convex for minimization, concave for maximization, or affine
        for find / feasibility problems.
    """
    def __init__(self, sense, expr):
        self.sense = sense
        self.expr = expr
        self.shape = expr.shape # better be scalar!
        
        if sense == 'minimize':
            self.is_dcp = isconvex(expr)
        elif sense == 'maximize':
            self.is_dcp = isconcave(expr)
        elif sense == 'find':
            self.is_dcp = isaffine(expr)
        else:
            self.is_dcp = False
    
    def __str__(self): return "%s %s" % (self.sense, self.expr)
    
    def children(self):
        nodelist = []
        if self.expr is not None: nodelist.append(("expr", self.expr))
        return tuple(nodelist)

    attr_names = ('is_dcp','shape')
    
class RelOp(Node):
    """ RelOp AST node.
    
        This node forms a constraint in the program.
        
        DCP compliance depends on the op type. For EQ, RelOp is DCP compliant
        when both sides are Affine. For GEQ, RelOp is DCP compliant when the 
        lefthand side is Concave and the righthand side is Convex. For LEQ,
        RelOp is DCP complaint when the lefthand side is Convex and the
        righthand side is Concave.
    """
    def __init__(self, op, left, right):
        self.op = op
        self.left = left
        self.right = right
        self.shape = left.shape + right.shape
        
        if op == '==':
            self.is_dcp = isaffine(left) and isaffine(right)
        elif op == '<=':
            self.is_dcp = isconvex(left) and isconcave(right)
        elif op == '>=':
            self.is_dcp = isconcave(left) and isconvex(right)
        else:
            self.is_dcp = False
    
    def __str__(self): return "%s %s %s" % (self.left, self.op, self.right)

    def children(self):
        nodelist = []
        if self.left is not None: nodelist.append(("left", self.left))
        if self.right is not None: nodelist.append(("right", self.right))
        return tuple(nodelist)

    attr_names = ('op', 'is_dcp', 'shape')

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
    
    def __str__(self): return "%s + %s" % (self.left, self.right)

    def children(self):
        nodelist = []
        if self.left is not None: nodelist.append(("left", self.left))
        if self.right is not None: nodelist.append(("right", self.right))
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
        if isparameter(self.left) or isconstant(self.left):
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
            raise TypeError("Not DCP compliant multiply %s * %s (lefthand side should be Constant or Parameter)" % (repr(left), repr(right)))

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
        # get the attributes of the atom
        try:
            self.sign, self.vexity, self.shape = scoop.atoms[self.name].attributes(*self.arglist)
        except TypeError as e:
            msg = re.sub(r'attributes\(\)', r'%s' % self.name, str(e))
            raise TypeError(msg)  
    
    def __str__(self): return "%s(%s)" % (self.name, ','.join(map(str, self.arglist)))
    
    def children(self):
        nodelist = []
        if self.arglist is not None: 
            for arg in self.arglist:
                nodelist.append(("argument", arg))
        return tuple(nodelist)

    attr_names = ('name', 'vexity', 'sign', 'shape')

# class For(Node):
#     def __init__(self, init, cond, next, stmt, coord=None):
#         self.init = init
#         self.cond = cond
#         self.next = next
#         self.stmt = stmt
#         self.coord = coord
# 
#     def children(self):
#         nodelist = []
#         if self.init is not None: nodelist.append(("init", self.init))
#         if self.cond is not None: nodelist.append(("cond", self.cond))
#         if self.next is not None: nodelist.append(("next", self.next))
#         if self.stmt is not None: nodelist.append(("stmt", self.stmt))
#         return tuple(nodelist)
# 
#     attr_names = ()

# class Assignment(Node):
#     def __init__(self, op, lvalue, rvalue, coord=None):
#         self.op = op
#         self.lvalue = lvalue
#         self.rvalue = rvalue
#         self.coord = coord
# 
#     def children(self):
#         nodelist = []
#         if self.lvalue is not None: nodelist.append(("lvalue", self.lvalue))
#         if self.rvalue is not None: nodelist.append(("rvalue", self.rvalue))
#         return tuple(nodelist)
# 
#     attr_names = ('op',)

