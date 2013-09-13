""" This is the geo_mean atom.

        geo_mean(x,y) = sqrt(x * y)

    If either x or y is a vector, the atom is applied elementwise.

    It is a CONCAVE atom. It is DECREASING in the first argument, and
    DECREASING in the second argument.

    It returns a SCALAR expression if the both arguments are SCALAR.
    Otherwise, it returns a VECTOR expression (sized to match the largest
    arugment).

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
import atom
from utils import *

class QC_geo_mean(atom.Atom):
    def __init__(self, x, y):
        super(QC_geo_mean, self).__init__(x,y)

    def _monotonicity(self):
        return [monotonicity.increasing, monotonicity.increasing]

    def _curvature(self):
        return curvature.Concave()

    def _sign(self):
        return sign.Positive()

    def _shape(self):
        return self.args[0].shape + self.args[1].shape

    def _canonicalize(self):
        v = Variable('', self.shape)
        x, y = self.args
        constraints = [
            SOCProd(x + y, [y - x, Number(2.0)*v]),
            y >= Number(0),
            x >= Number(0)
        ]
        return (v, constraints)

# register with the atom library
atom.atoms['geo_mean'] = QC_geo_mean

