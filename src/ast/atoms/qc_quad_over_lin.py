""" This is the quad_over_lin atom.

        quad_over_lin(x,y) = (x^Tx) ./ y

    If y is a vector, it computes the division elementwise.

    It is a CONVEX atom. It is NONMONOTONE in the first argument, and
    DECREASING in the second argument.

    If the first argument is POSITIVE, it is INCREASING in the first argument.
    If the first argument is NEGATIVE, it is DECRASING in the first argument.

    It returns a SCALAR expression if the second argument is SCALAR.
    Otherwise, it returns a VECTOR expression (sized to match the second
    arugment).

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
import atom
from utils import *

class QC_quad_over_lin(atom.Atom):
    def __init__(self, x, y):
        super(QC_quad_over_lin, self).__init__(x, y)

    def _monotonicity(self):
        return [monotonicity.signed(self.args[0]), monotonicity.decreasing]

    def _curvature(self):
        return curvature.Convex()

    def _sign(self):
        return sign.Positive()

    def _shape(self):
        if shape.isscalar(self.args[1]): return shape.Scalar()
        else: raise TypeError("Cannot use quad_over_lin with vector y arguments. Perhaps you meant 'square_over_lin'?")


    def _canonicalize(self):
        v = Variable('', self.shape)
        x,y = self.args
        constraints = [
            SOC(y + v, [y - v, Number(2.0)*x]),
            y >= Number(0)
        ]
        return (v, constraints)

# register with the atom library
atom.atoms['quad_over_lin'] = QC_quad_over_lin
