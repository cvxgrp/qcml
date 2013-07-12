# Built from PyCParser's AST and Python's AST

from qc_vexity import Convex, Concave, Affine, Nonconvex, isaffine, isconvex, isconcave
from qc_shape import scalar, isscalar
import sys, operator



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
            if isinstance(child, list):
                for c in child:
                    c.show(
                        buf,
                        offset=offset + 2,
                        attrnames=attrnames,
                        nodenames=nodenames,
                        _my_node_name=child_name)
            else:
                child.show(
                    buf,
                    offset=offset + 2,
                    attrnames=attrnames,
                    nodenames=nodenames,
                    _my_node_name=child_name)

class NodeVisitor(object):
    """
    A node visitor base class that walks the abstract syntax tree and calls a
    visitor function for every node found.  This function may return a value
    which is forwarded by the `visit` method.

    This class is meant to be subclassed, with the subclass adding visitor
    methods.

    Per default the visitor functions for the nodes are ``'visit_'`` +
    class name of the node.  So a `TryFinally` node visit function would
    be `visit_TryFinally`.  This behavior can be changed by overriding
    the `visit` method.  If no visitor function exists for a node
    (return value `None`) the `generic_visit` visitor is used instead.

    Don't use the `NodeVisitor` if you want to apply changes to nodes during
    traversing.  For this a special visitor exists (`NodeTransformer`) that
    allows modifications.
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
            if isinstance(c, list):
                for elem in c:
                    if isinstance(elem, Node):
                        self.visit(elem)
            elif isinstance(c, Node):
                self.visit(c)

class NodeTransformer(NodeVisitor):
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
    def generic_visit(self, node):
        """ Called if no explicit visitor function exists for a
            node. Implements preorder visiting of the node.
        """
        for c_name, c in node.children():
            if isinstance(c, list):
                new_values = []
                for elem in c:
                    if isinstance(elem, Node):
                        elem = self.visit(elem)
                        if elem is None:
                            continue
                        elif not isinstance(elem, Node):
                            # returns a list
                            new_values.extend(elem)
                            continue
                    new_values.append(elem)
                c[:] = new_values
            elif isinstance(c, Node):
                new_node = self.visit(c)
                if new_node is None:
                    delattr(node, c_name)
                else:
                    setattr(node, c_name, new_node)
        return node

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
        if self.constraints is not None: nodelist.append(("constraints" , self.constraints))
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

    def __eq__(self, other):
        if self.op == other.op:
            return str(self.left - self.right) == str(other.left - other.right)
        elif self.op == '<=' and other.op == '>=':
            return str(self.left - self.right) == str(other.right - other.left)
        elif self.op == '>=' and other.op == '<=':
            return str(self.right - self.left) == str(other.left - other.right)
        else:
            return False

    def __ne__(self, other):
        return not (self == other)

    def __hash__(self):
        if self.op == '<=':
            return hash(str(self.left - self.right))
        else:
            return hash(str(self.right - self.left))

    def __str__(self): return "%s %s %s" % (self.left, self.op, self.right)

    def children(self):
        nodelist = []
        if self.left is not None: nodelist.append(("left", self.left))
        if self.right is not None: nodelist.append(("right", self.right))
        return tuple(nodelist)

    attr_names = ('op', 'is_dcp', 'shape')

class SOC(Node):
    """ SOC AST node.

        This node forms a second-order cone constraint in the program.

        norm([x;y;z]) <= t

        It is DCP compliant when x,y,z, and t are Affine expressions.
    """
    def __init__(self, t, args):
        self.left = args
        self.right = t
        self.shape = scalar()

        if not isscalar(self.right):
            raise TypeError("Cannot form SOC constraint with vector '%s' right-hand side." % self.right)

        self.is_dcp = all(map(isaffine, self.left)) and isaffine(self.right)

    def __str__(self): return "norm([%s]) <= %s" % ('; '.join(map(str,self.left)), self.right)

    def __eq__(self, other):
        return str(self) == str(other)

    def __ne__(self, other):
        return not (self == other)

    def __hash__(self):
        return hash(str(self))

    def children(self):
        nodelist = []
        if self.right is not None: nodelist.append(("right", self.right))
        if self.left is not None: nodelist.append(("left", self.left))
        return tuple(nodelist)

    attr_names = ('is_dcp', 'shape')

class SOCProd(Node):
    """ SOCProd AST node.

        This node forms a product of second-order cone constraints in the
        program.

        norm([x1;y1;z1]) <= t1
        norm([x2;y2;z2]) <= t2
        ...

        More concisely, it is written as

        norm(x,y,z) <= t

        where x = (x1,x2,..), y = (y1,y2,..), and so on

        It is DCP compliant when the arguments and t are Affine expressions.
    """
    def __init__(self, t, args):
        self.arglist = args
        self.right = t
        self.shape = reduce(operator.add, map(lambda x: x.shape, args), self.right.shape)

        self.is_dcp = all(map(isaffine, args)) and isaffine(self.right)

    def __str__(self):
        if len(self.arglist) == 1:
            return "abs(%s) <= %s" % (self.arglist[0], self.right)
        else:
            return "norm(%s) <= %s" % (', '.join(map(str, self.arglist)), self.right)

    def __eq__(self, other):
        return str(self) == str(other)

    def __ne__(self, other):
        return not (self == other)

    def __hash__(self):
        return hash(str(self))

    def children(self):
        nodelist = []
        if self.right is not None: nodelist.append(("right", self.right))
        if self.arglist is not None: nodelist.append(("arglist", self.arglist))
        return tuple(nodelist)

    attr_names = ('is_dcp', 'shape')




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

