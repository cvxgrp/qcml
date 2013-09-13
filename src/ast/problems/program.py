from .. node import Node
import objective as obj
from .. constraints import *
from .. expressions import Variable

class Program(Node):
    """ Program node.

        This node is root node for the program. Its children is a *single*
        Objective node and a list of RelOp nodes.

        It is DCP compliant when the Objective is DCP and the RelOp's are DCP.
    """
    def __init__(self, objective, constraints, variables, parameters={}, dimensions=set()):
        assert(isinstance(objective, obj.Objective))
        assert(all(isinstance(x, Constraint) for x in constraints))

        self.objective = objective

        # adds constraints to the proper set (using constr_map) and removes
        # duplicates
        self.eq_constr, self.ineq_constr, self.soc_constr = set(), set(), set()
        self.__constr_map = {
            LinearEquality: self.eq_constr.add,
            LinearInequality: self.ineq_constr.add,
            SOC: self.soc_constr.add,
            SOCProd: self.soc_constr.add,
            None.__class__: lambda x: None,   # if None, don't do anything
        }
        for c in constraints: self.__constr_map[c.__class__](c)

        self.is_dcp = objective.is_dcp and all(c.is_dcp for c in constraints)
        self.variables = variables     # variables declared by the user
        self.parameters = parameters   # parameters declared by the user
        self.dimensions = dimensions   # dimensions declared by the user
        self.new_variables = {}        # new variables introduced by rewriting

    def __str__(self):
        constrs = map(str, self.eq_constr) + map(str, self.ineq_constr) + map(str, self.soc_constr)
        return "%s\nsubject to\n    %s" % (self.objective, '\n    '.join(constrs))

    def info(self):
        if self.is_dcp: return "DCP program:"
        else: return "Non-DCP program:"

    def canonicalize(self):
        constraints = []
        for c in self.children():
            _, constr = c.canonicalize()
            constraints.extend(expr.simplify() for expr in constr)

        # clear the constraint sets
        map(lambda x: x.clear(), (self.eq_constr, self.ineq_constr, self.soc_constr))
        for c in constraints: self.__constr_map[c.__class__](c)

        # doesn't actually need to return anything...
        #return self.objective, filter(None, constraints)
        self.new_variables = Variable.anonymous_variables

    def children(self):
        """ An iterator that yields the children
        """
        yield self.objective
        for c in self.eq_constr: yield c
        for c in self.ineq_constr: yield c
        for c in self.soc_constr: yield c

    def add_constraint(self, c):
        """ Allows us to add constraints to the program
        """
        self.__constr_map[c.__class__](c)


# test_program


