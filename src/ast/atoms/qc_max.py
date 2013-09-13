""" This is the max atom.

        max(x) = maximum element of x

    It is a CONVEX atom. It is INCREASTING in the first argument.

    It returns a SCALAR expressions. If multiple arguments are supplied, it
    compares them elementwise and returns a VECTOR expressions.

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
import atom
from utils import *

class QC_max(atom.Atom):
    def __init__(self, *args):
        super(QC_max, self).__init__(*args)

    def _monotonicity(self):
        return [monotonicity.increasing]*len(self.args)

    def _curvature(self):
        return curvature.Convex()

    def _sign(self):
        if any(sign.ispositive(e) for e in self.args): return sign.Positive()
        if all(sign.isnegative(e) for e in self.args): return sign.Negative()
        return sign.Neither()

    def _shape(self):
        if len(self.args) == 1: return shape.Scalar()
        else:
            base_shape = shape.Scalar()
            for e in self.args:
                base_shape += e.shape
            return base_shape

    def _canonicalize(self):
        v = Variable('', self.shape)
        constraints = [v >= x for x in self.args]
        return (v, constraints)

# register with the atom library
atom.atoms['max'] = QC_max
