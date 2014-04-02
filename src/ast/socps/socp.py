""" An SOCP object.
"""

from .. import Node, NodeVisitor
from . program_objective import ProgramObjective
from . program_data import ProgramData
from . program_constraints import ProgramConstraints


class DimsSetter(NodeVisitor):
    """ Goal is to traverse the tree and call eval(dims) on every
        possible shape.
    """
    def __init__(self, dims):
        self.dims = dims

    def generic_visit(self, node):
        if hasattr(node, 'shape'):
            node.shape.eval(self.dims)
        super(DimsSetter, self).generic_visit(node)


class SOCP(Node):
    """ 9/15/2103

        It's possible that instead of "Program", I'm going to want to have
        SOCP.

        Instead of "canonicalize()", I'll have "canonical_form()" which will
        return an SOCP.

        Then, we define operations on SOCPs, so canonicalizing is just a
        matter of returning canonical forms and performing the needed
        operation (+, *, -, etc.)

        For instance, *any* op concatenates constraints. The op is only
        applied to the objective. Furthermore, an SOCP assumes the objective
        is a minimization...

        An SOCP will contain
            -- an objective
            -- constraints
            -- data

        This is the *root* expression node.
    """
    # number of new variables introduced so far
    count = 0
    # new variables introduced by canonicalization
    new_variables = {}

    def __init__(self, objective, constraints, data):
        assert(isinstance(objective, ProgramObjective))
        assert(isinstance(constraints, ProgramConstraints))
        assert(isinstance(data, ProgramData))

        self.is_dcp = objective.is_dcp and constraints.is_dcp

        self.objective = objective
        self.constraints = constraints
        self.data = data

    def __str__(self):
        constr = '\n    '.join(str(self.constraints).split('\n'))
        return "%s\nsubject to\n    %s" % (self.objective, constr)

    def info(self):
        prob = '\n    '.join(str(self).split('\n'))
        if self.is_dcp:
            return "DCP problem:\n    %s" % (prob,)
        else:
            return "Non-DCP problem:\n    %s" % (prob,)

    # TODO: below is "future"
    # def canonical_form(self):
    #     children = self.children()
    #     p = children.next().canonical_form()
    #     for child in children:
    #         p += child.canonical_form()  # this returns an SOCP
    #     return p

    # TODO: old canonicalize
    def canonicalize(self):
        SOCP.reset()
        _, constraints = self.objective.canonicalize()
        self.constraints.canonicalize()

        for constr in constraints:
            self.constraints.add(constr)

    def children(self):
        """ An iterator that yields a program's children
        """
        yield self.objective
        yield self.constraints

    @classmethod
    def reset(cls):
        cls.count = 0
        cls.new_variables = {}

    @property
    def parameters(self):
        return self.data.parameters

    @property
    def variables(self):
        return self.data.variables

    @property
    def dimensions(self):
        return self.data.dimensions

    @property
    def abstract_dims(self):
        return self.data.abstract_dims

    @dimensions.setter
    def dimensions(self, dims):
        for elem in self.new_variables.values():
            elem.shape.eval(dims)
        DimsSetter(dims).generic_visit(self)
        self.data.dimensions = dims




