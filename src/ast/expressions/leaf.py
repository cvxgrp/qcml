import expression as e

from ... properties import sign, shape, curvature

class Number(e.LeafMixin, e.Expression):
    """ Number AST node.

        Contains a floating point number. It is Affine; its sign depends on
        the sign of the float.
    """
    def __init__(self, value):
        if float(value) >= 0.0: my_sign = sign.Positive()
        else: my_sign = sign.Negative()
        super(Number, self).__init__(value = value, curvature = curvature.Constant(), shape = shape.Scalar(), sign = my_sign)

    def __repr__(self): return "Number(%s)" % self.value

class Parameter(e.LeafMixin, e.Expression):
    """ Parameter AST node.

        Contains a representation of Parameters. It is Affine; its sign and
        shape are supplied from QCML.
    """
    def __init__(self, value, shape, sign):
        super(Parameter, self).__init__(value = value, curvature = curvature.Constant(), shape = shape, sign = sign)

    def __repr__(self): return "Parameter('%s',%s)" % (self.value, self.shape)


