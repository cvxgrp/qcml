import qcml.expressions.ast as ast
from abc import ABCMeta, abstractmethod, abstractproperty

class Constraint(ast.Node):
    """ Constraint AST node.

        This node forms a constraint in the program.

        DCP compliance depends on the op type. For EQ, RelOp is DCP compliant
        when both sides are Affine. For GEQ, RelOp is DCP compliant when the
        lefthand side is Concave and the righthand side is Convex. For LEQ,
        RelOp is DCP complaint when the lefthand side is Convex and the
        righthand side is Concave.
    """
    __metaclass__ = ABCMeta

    def __init__(self, left, right, shape, is_dcp):
        self.left = left
        self.right = right
        self.shape = shape
        self.is_dcp = is_dcp

    @abstractmethod
    def __str__(self):
        pass

    def __eq__(self, other):
        # do we simplify before comparing?
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

    @abstractmethod
    def canonicalize(self):
        pass

    @abstractmethod
    def simplify(self):
        pass

    # def canonicalize(self):
    #     self.left, constraints = map(list, zip(*[elem.canonicalize() for elem in self.left]))
    #     self.right, constraint = self.right.canonicalize()
    #     constr = constraint
    #     for c in constraints:
    #         constr += c
    #     return (None, [self] + constr)
    #
    # def simplify(self):
    #     self.left = [elem.simplify() for elem in self.left]
    #     self.right = self.right.simplify()
    #     return self

    attr_names = ('is_dcp', 'shape')



