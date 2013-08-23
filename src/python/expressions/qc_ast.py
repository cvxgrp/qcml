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

