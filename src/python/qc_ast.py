# Built from PyCParser's AST and Python's AST
from qc_vexity import Convex, Concave, Affine, Nonconvex
from qc_sign import Positive, Negative, Neither
from qc_shape import Scalar, Vector, Matrix
from utils import isaffine, isconvex, isconcave, ispositive, isnegative, isconstant, isparameter
import sys

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


class NodeVisitor(object):
    """ A base NodeVisitor class for visiting c_ast nodes. 
        Subclass it and define your own visit_XXX methods, where
        XXX is the class name you want to visit with these 
        methods.
        
        For example:
        
        class ConstantVisitor(NodeVisitor):
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
                NodeVisitor.generic_visit(self, node)
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


class Program(Node):
    def __init__(self, objective, constraints, variables, parameters, dimensions):
        self.objective = objective
        self.constraints = constraints
        self.is_dcp = objective.is_dcp and all(c.is_dcp for c in constraints)
        self.variables = variables      # variables declared by the user
        self.parameters = parameters    # parameters declared by the user
        self.dimensions = dimensions    # dimensions declared by the user
        self.new_variables = {}         # new variables introduced by rewriting
        
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


class Constant(Node):
    def __init__(self, value):
        self.value = value  # this is a float
        self.vexity = Affine()
        if float(value) >= 0.0:
            self.sign = Positive()
        else:
            self.sign = Negative()
        self.shape = Scalar()
            
    def __str__(self): return str(self.value)
        
    def children(self): return []
    
    attr_names = ('value', 'vexity', 'sign')

class Parameter(Node):
    def __init__(self, value, shape, sign):
        self.value = value
        self.vexity = Affine()
        self.sign = sign
        self.shape = shape
    
    def __str__(self): return str(self.value)
    
    def children(self): return []
    
    attr_names = ('value', 'vexity', 'sign','shape')
    
class Variable(Node):
    def __init__(self, value, shape):
        self.value = value
        self.vexity = Affine()
        self.sign = Neither()
        self.shape = shape
        
    def __str__(self): return str(self.value)
        
    def children(self): return []
    
    attr_names = ('value', 'vexity', 'sign', 'shape')

class Add(Node):
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

    def children(self):
        nodelist = []
        if self.left is not None: nodelist.append(("left", self.left))
        if self.right is not None: nodelist.append(("right", self.right))
        return tuple(nodelist)

    attr_names = ('vexity', 'sign','shape')

class RelOp(Node):
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

    def children(self):
        nodelist = []
        if self.left is not None: nodelist.append(("left", self.left))
        if self.right is not None: nodelist.append(("right", self.right))
        return tuple(nodelist)

    attr_names = ('op', 'is_dcp', 'shape')
    
class Mul(Node):
    """ Assumes the lefthand side is a Constant or a Parameter
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

    def children(self):
        nodelist = []
        if self.right is not None: nodelist.append(("right", self.right))
        return tuple(nodelist)

    attr_names = ('left', 'vexity', 'sign', 'shape')

class Negate(Node):
    def __init__(self, expr):
        self.expr = expr
        self.sign = -expr.sign
        self.vexity = -expr.vexity
        self.shape = expr.shape

    def children(self):
        nodelist = []
        if self.expr is not None: nodelist.append(("expr", self.expr))
        return tuple(nodelist)

    attr_names = ('vexity', 'sign', 'shape')

class Transpose(Node):
    def __init__(self, expr):
        self.expr = expr
        self.sign = expr.sign
        self.vexity = expr.vexity
        self.shape = expr.shape.transpose()

    def children(self):
        nodelist = []
        if self.expr is not None: nodelist.append(("expr", self.expr))
        return tuple(nodelist)

    attr_names = ('vexity', 'sign', 'shape')
    
class Objective(Node):
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
    
    def children(self):
        nodelist = []
        if self.expr is not None: nodelist.append(("expr", self.expr))
        return tuple(nodelist)

    attr_names = ('is_dcp','shape')

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

