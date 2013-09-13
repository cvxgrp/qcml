""" This is the huber atom.

        huber(x) = minimize(w.^2 + 2*v) s.t. (abs(x) <= w + v; w<=1; v>=0)

    If x is a vector, it computes the elementwise huber.

    It is a CONVEX atom. It is NONMONOTONE in the first argument.

    If the first argument is POSITIVE, it is INCREASING in the first argument.
    If the first argument is NEGATIVE, it is DECRASING in the first argument.

    It returns a VECTOR expressions.

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
import atom
from utils import *

from qc_square import QC_square
from qc_abs import QC_abs

class QC_huber(atom.Atom):
    def __init__(self, x):
        super(QC_huber, self).__init__(x)

    def _monotonicity(self):
        return [monotonicity.signed(self.args[0])]

    def _curvature(self):
        return curvature.Convex()

    def _sign(self):
        return sign.Positive()

    def _shape(self):
        return self.args[0].shape

    def _canonicalize(self):
        w, v = Variable('', self.shape), Variable('', self.shape)
        x, = self.args

        o1, c1 = QC_square(w).canonicalize()
        constraints = [
            QC_abs(x) <= w + v,
            w <= Number(1),
            v >= Number(0)
        ] + c1
        # w^2 + 2*v
        return (o1 + Number(2)*v, constraints)

# register with the atom library
atom.atoms['huber'] = QC_huber
