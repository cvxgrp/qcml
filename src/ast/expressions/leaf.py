import expression as e

from qcml.properties import sign, shape, curvature

class Number(e.Expression, e.Leaf):
    """ Number AST node.

        Contains a floating point number. It is Affine; its sign depends on
        the sign of the float.
    """
    def __init__(self, value):
        if float(value) >= 0.0: my_sign = sign.Positive()
        else: my_sign = sign.Negative()
        super(Number, self).__init__(value = value, curvature = curvature.Constant(), shape = shape.Scalar(), sign = my_sign)

    def __repr__(self): return "Number(%s)" % self.value

class Parameter(e.Expression, e.Leaf):
    """ Parameter AST node.

        Contains a representation of Parameters. It is Affine; its sign and
        shape are supplied from QCML.
    """
    def __init__(self, value, shape, sign):
        super(Parameter, self).__init__(value = value, curvature = curvature.Constant(), shape = shape, sign = sign)

    def __repr__(self): return "Parameter('%s',%s)" % (self.value, self.shape)

class Variable(e.Expression, e.Leaf):
    """ Variable AST node.

        Contains a representation of Variables. It is Affine; its sign is
        Neither positive nor negative. Its shape is supplied from QCML.
    """
    count = 0
    anonymous_variables = {}
    def __init__(self, value, shape):
        if value:
            name = value
        else:
            name = "_t%d" % Variable.count
            Variable.count += 1
            Variable.anonymous_variables[name] = self
        super(Variable, self).__init__(value = name, curvature = curvature.Affine(), shape = shape, sign = sign.Neither())

    def __repr__(self): return "Variable('%s',%s)" % (self.value, self.shape)

    @classmethod
    def reset(cls):
        cls.count = 0
        cls.anonymous_variables = {}
