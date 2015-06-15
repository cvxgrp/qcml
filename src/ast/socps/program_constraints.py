""" ProgramConstraints is an object that contains a list of constraints for
    an SOCP.
"""
from .. node import Node
from .. constraints import LinearEquality, LinearInequality, \
    SOC, SOCProd, Constraint

class ProgramConstraints(Node):
    """ ProgramConstraints object.

        This object lives inside an SOCP. It contains the list of constraints
        and a method to add to them as well as iterate over them.
    """
    def __init__(self, constraints):
        """ Initalizes the object. Builds a set of constraints so that
            redundant constraints (as determined by their string description)
            are removed.
        """
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

    # def __iter__(self):
    #     """ An iterator that yields the constraints
    #     """
    #     for constr in self.eq_constr:
    #         yield constr
    #     for constr in self.ineq_constr:
    #         yield constr
    #     for constr in self.soc_constr:
    #         yield constr

    def __str__(self):
        """ Returns a formatted string for the constraints.
        """
        return '\n'.join([str(x) for x in self.children()])

    def add(self, constraint):
        """ Allows us to add constraints to the program.
        """
        self.__constr_map[constraint.__class__](constraint)

    def clear(self):
        """ Clears the constraint set.
        """
        self.eq_constr.clear()
        self.ineq_constr.clear()
        self.soc_constr.clear()

    def info(self):
        """ The description to show in the AST for debugging.
        """
        if self.is_dcp:
            return "DCP constraints:"
        else:
            return "Non-DCP constraints:"

    def children(self):
        """ The children for traversing the AST.
        """
        for constr in self.eq_constr:
            yield constr
        for constr in self.ineq_constr:
            yield constr
        for constr in self.soc_constr:
            yield constr

    # TODO: old canonicalize
    def canonicalize(self):
        constraints = []
        for child in self.children():
            _, constr = child.canonicalize()
            constraints.extend(expr.simplify() for expr in constr)

        self.clear()
        for constr in constraints:
            self.add(constr)
