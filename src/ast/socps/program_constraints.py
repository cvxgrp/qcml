""" ProgramConstraints is an object that contains a list of constraints for
    an SOCP.
"""
from .. constraints import LinearEquality, LinearInequality, \
    SOC, SOCProd, Constraint

class ProgramConstraints(object):
    """ ProgramConstraints object.

        This object lives inside an SOCP. It contains the list of constraints
        and a method to add to them as well as iterate over them.
    """
    def __init__(self, constraints):
        assert(all(isinstance(x, Constraint) for x in constraints))

        self.is_dcp = all(c.is_dcp for c in constraints)
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

    def __iter__(self):
        """ An iterator that yields the constraints
        """
        for constr in self.eq_constr:
            yield constr
        for constr in self.ineq_constr:
            yield constr
        for constr in self.soc_constr:
            yield constr

    def __str__(self):
        return '\n    '.join([str(x) for x in self])

    def add(self, constraint):
        """ Allows us to add constraints to the program
        """
        self.__constr_map[constraint.__class__](constraint)

    def clear(self):
        # clear the constraint sets
        self.eq_constr.clear()
        self.ineq_constr.clear()
        self.soc_constr.clear()
