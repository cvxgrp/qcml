""" A program consists of a single problem, the variables, the parameters,
    and the dimensions associated with the program.

    It provides several visitors to modify the underlying problem:
        DimsSetter -- visits each node in the program to set the dimensions
        Canonicalizer -- rewrites the problem nodes as needed
"""
from . import NodeVisitor

class DimsSetter(NodeVisitor):
    """ Goal is to traverse the tree and call eval(dims) on every
        possible shape.
    """
    def __init__(self, dims):
        self.dims = dims

    def generic_visit(self, node):
        if hasattr(node, 'shape'): node.shape.eval(self.dims)
        super(DimsSetter, self).generic_visit(node)


class Program(object):
    """ A Program contains the problem and the variables, parameters, and
        dimensions involved in the problem.

        You are able to set the dimensions of the problem (via .dimensions).
        And you are able to canonicalize the problem.
    """

    # number of new variables introduced so far
    count = 0
    # new variables introduced by canonicalization
    new_variables = {}
    # new constraints introduced by canonicalization
    new_constraints = []

    def __init__(self, problem, variables, parameters = None, dimensions = None):
        self.problem = problem

        self.is_dcp = self.problem.is_dcp
        # dimensions declared by the user
        self.__dimensions = dimensions if dimensions else set()
        # parameters declared by the user
        self.parameters = parameters if parameters else {}
        # variables declared by the user
        self.variables = variables

    @classmethod
    def reset(cls):
        cls.count = 0
        cls.new_variables = {}

    def canonicalize(self):
        """ This canonicalizes the problem.

            Although it looks innocent, it mutates the state of the Program
            by modifying the *class attributes* new_variables and
            new_constraints.
        """
        self.problem.canonicalize()

    def show(self, *args):
        self.problem.show(*args)

    def codegen(self):
        """ This generates code....
        """
        pass

    @property
    def dimensions(self):
        return self.__dimensions

    @dimensions.setter
    def dimensions(self, dims):
        for v in self.variables.values():
            v.shape.eval(dims)
        for v in self.new_variables.values():
            v.shape.eval(dims)
        for v in self.parameters.values():
            v.shape.eval(dims)
        DimsSetter(dims).generic_visit(self.problem)
        self.__dimensions = dims


