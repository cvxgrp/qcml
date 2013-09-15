""" A problem....
"""

from .. import Node, Program
from . objective import Objective
from .. constraints import LinearEquality, LinearInequality, \
    SOC, SOCProd, Constraint
from .. expressions import Variable

class Problem(Node):
    """ Problem node.

        Its children is a *single* Objective node and a list of constraint
        nodes.

        It is DCP compliant when the Objective is DCP and the RelOp's are DCP.
    """
    def __init__(self, objective, constraints):
        assert(isinstance(objective, Objective))
        assert(all(isinstance(x, Constraint) for x in constraints))

        self.is_dcp = objective.is_dcp and all(c.is_dcp for c in constraints)
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
        for constr in constraints:
            self.__constr_map[constr.__class__](constr)

        super(Problem, self).__init__()

    def __str__(self):
        constrs = '\n    '.join([str(x) for x in self.eq_constr] + \
            [str(x) for x in self.ineq_constr] + \
            [str(x) for x in self.soc_constr])
        return "%s\nsubject to\n    %s" % (self.objective, constrs)

    def info(self):
        if self.is_dcp:
            return "DCP program:"
        else:
            return "Non-DCP program:"

    def canonicalize(self):
        Program.reset()
        constraints = []
        for child in self.children():
            _, constr = child.canonicalize()
            constraints.extend(expr.simplify() for expr in constr)

        # clear the constraint sets
        self.eq_constr.clear()
        self.ineq_constr.clear()
        self.soc_constr.clear()
        for constr in constraints:
            self.__constr_map[constr.__class__](constr)


    def children(self):
        """ An iterator that yields the children
        """
        yield self.objective
        for constr in self.eq_constr:
            yield constr
        for constr in self.ineq_constr:
            yield constr
        for constr in self.soc_constr:
            yield constr

    def add_constraint(self, constraint):
        """ Allows us to add constraints to the program
        """
        self.__constr_map[constraint.__class__](constraint)
