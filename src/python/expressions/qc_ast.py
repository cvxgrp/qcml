# Built from PyCParser's AST and Python's AST
import ast
import qcml.properties.shape as shape
import qcml.properties.curvature as curvature
import expression as e
import operator

# TODO: ... I don't think these actually need to be ASTs...
# but i get the nice "printing" ability

class Program(ast.Node):
    """ Program AST node.

        This node is root node for the program. Its children is a *single*
        Objective node and a list of RelOp nodes.

        It is DCP compliant when the Objective is DCP and the RelOp's are DCP.
    """
    def __init__(self, objective, constraints, variables, parameters={}, dimensions=set()):
        self.objective = objective
        self.constraints = constraints
        self.is_dcp = objective.is_dcp and all(c.is_dcp for c in constraints)
        self.variables = variables     # variables declared by the user
        self.parameters = parameters   # parameters declared by the user
        self.dimensions = dimensions   # dimensions declared by the user
        self.new_variables = {}        # new variables introduced by rewriting

    def __str__(self): return "%s\nsubject to\n    %s" % (self.objective, '\n    '.join(map(str,self.constraints)))

    def children(self):
        nodelist = []
        if self.objective is not None: nodelist.append(("objective", self.objective))
        if self.constraints is not None: nodelist.append(("constraints" , self.constraints))
        return tuple(nodelist)

    def canonicalize(self):
        obj, constraints = self.objective.canonicalize()
        constraints = [c.simplify() for c in constraints]
        for c in self.constraints:
            _, constr = c.canonicalize()
            constraints.extend(expr.simplify() for expr in constr)

        self.objective.expr = obj.simplify()
        self.constraints = filter(None, constraints)
        return self.objective, self.constraints

    def add_constraint(self, c):
        """ Allows us to add constraints to the program
        """
        self.constraints.append(c)

    attr_names = ('is_dcp',)

class Objective(ast.Node):
    """ Objective AST node.

        This node contains the representation of the objective. It also stores
        the objective's sense (minimize, maximize, or find).

        It is DCP compliant when the objective's expression agrees with its
        sense. Convex for minimization, concave for maximization, or affine
        for find / feasibility problems.
    """
    def __init__(self, sense, expr):
        assert(shape.isscalar(expr))

        self.sense = sense
        self.expr = expr
        self.shape = expr.shape # better be scalar!

        if sense == 'minimize':
            self.is_dcp = curvature.isconvex(expr)
        elif sense == 'maximize':
            self.is_dcp = curvature.isconcave(expr)
        elif sense == 'find':
            self.is_dcp = curvature.isaffine(expr)
        else:
            self.is_dcp = False

    def __str__(self): return "%s %s" % (self.sense, self.expr)

    def children(self):
        nodelist = []
        if self.expr is not None: nodelist.append(("expr", self.expr))
        return tuple(nodelist)

    attr_names = ('is_dcp','shape')

    def canonicalize(self):
        return self.expr.canonicalize()

class RelOp(ast.Node):
    # TODO: make it a linear ineq, with only a "left" <= 0
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
            self.is_dcp = curvature.isaffine(left) and curvature.isaffine(right)
        elif op == '<=':
            self.is_dcp = curvature.isconvex(left) and curvature.isconcave(right)
        elif op == '>=':
            self.is_dcp = curvature.isconcave(left) and curvature.isconvex(right)
        else:
            self.is_dcp = False

    def __eq__(self, other):
        if self.op == other.op:
            return str( (self.left - self.right).simplify() ) == str( (other.left - other.right).simplify() )
        elif self.op == '<=' and other.op == '>=':
            return str( (self.left - self.right).simplify() ) == str( (other.right - other.left).simplify() )
        elif self.op == '>=' and other.op == '<=':
            return str( (self.right - self.left).simplify() ) == str( (other.left - other.right).simplify() )
        else:
            return False

    def __ne__(self, other):
        return not (self == other)

    def __hash__(self):
        if self.op == '<=':
            return hash(str( (self.left - self.right).simplify() ))
        else:
            return hash(str( (self.right - self.left).simplify() ))

    def __str__(self): return "%s %s %s" % (self.left, self.op, self.right)

    def children(self):
        nodelist = []
        if self.left is not None: nodelist.append(("left", self.left))
        if self.right is not None: nodelist.append(("right", self.right))
        return tuple(nodelist)

    attr_names = ('op', 'is_dcp', 'shape')

    def canonicalize(self):
        self.left, lh_constr = self.left.canonicalize()
        self.right, rh_constr = self.right.canonicalize()
        return (None, [self] + lh_constr + rh_constr)

    def simplify(self):
        self.left = self.left.simplify()
        self.right = self.right.simplify()
        return self

class SOC(ast.Node):
    """ SOC AST node.

        This node forms a second-order cone constraint in the program.

        norm([x;y;z]) <= t

        It is DCP compliant when x,y,z, and t are Affine expressions.
    """
    def __init__(self, t, args):
        self.left = args
        self.right = t
        self.shape = shape.Scalar()

        if not shape.isscalar(self.right):
            raise TypeError("Cannot form SOC constraint with vector '%s' right-hand side." % self.right)

        self.is_dcp = all(map(curvature.isaffine, self.left)) and curvature.isaffine(self.right)

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

    def canonicalize(self):
        self.left, constraints = map(list, zip(*[elem.canonicalize() for elem in self.left]))
        self.right, constraint = self.right.canonicalize()
        constr = constraint
        for c in constraints:
            constr += c
        return (None, [self] + constr)

    def simplify(self):
        self.left = [elem.simplify() for elem in self.left]
        self.right = self.right.simplify()
        return self

    attr_names = ('is_dcp', 'shape')

class SOCProd(ast.Node):
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

        self.is_dcp = all(map(curvature.isaffine, args)) and curvature.isaffine(self.right)

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

    def canonicalize(self):
        self.arglist, constraints = map(list, zip(*[elem.canonicalize() for elem in self.arglist]))
        self.right, constraint = self.right.canonicalize()
        constr = constraint
        for c in constraints:
            constr += c
        return (None, [self] + constr)

    def simplify(self):
        self.arglist = [elem.simplify() for elem in self.arglist]
        self.right = self.right.simplify()
        return self

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

