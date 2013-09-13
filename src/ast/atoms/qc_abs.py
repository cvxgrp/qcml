import atom
from utils import *

class QC_abs(atom.Atom):
    def __init__(self, x):
        super(QC_abs, self).__init__(x)

    def _monotonicity(self):
        return [monotonicity.signed(self.args[0])]

    def _curvature(self):
        return curvature.Convex()

    def _sign(self):
        return sign.Positive()

    def _shape(self):
        return self.args[0].shape

    def _canonicalize(self):
        x, = self.args
        v = Variable('',self.shape)
        constraints = [x <= v, x >= -v]
        return (v, constraints)

# register with the atom library
atom.atoms['abs'] = QC_abs
