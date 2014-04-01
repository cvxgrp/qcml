from . expression import LeafMixin, Expression
from ... properties import sign, curvature
from .. socps.socp import SOCP

class Variable(LeafMixin, Expression):
    """ Variable AST node.

        Contains a representation of Variables. It is Affine; its sign is
        Neither positive nor negative. Its shape is supplied from QCML.

        It uses a global variable counter from the SOCP ast object
        (see ast.socps.socp).
    """

    def __init__(self, name, shape):
        """ Create a variable with name "name" and with shape "shape".

            If no name is given, will create a new variable with an
            internal name, prefixed with "_t".
        """
        if not name:
            name = "_t%d" % SOCP.count
            SOCP.count += 1
            SOCP.new_variables[name] = self
        super(Variable, self).__init__(
            value = name,
            curvature = curvature.Affine(),
            shape = shape,
            sign = sign.Neither())

    def __repr__(self):
        return "Variable('%s',%s)" % (self.value, self.shape)
