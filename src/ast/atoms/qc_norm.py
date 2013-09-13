import atom
from utils import *

class QC_norm(atom.Atom):
    def __init__(self, *args):
        super(QC_norm, self).__init__(*args)

    def _monotonicity(self):
        return [monotonicity.signed(e) for e in self.args]

    def _curvature(self):
        return curvature.Convex()

    def _sign(self):
        return sign.Positive()

    def _shape(self):
        if len(self.args) == 1: return shape.Scalar()
        else:
            new_shape = shape.Scalar()
            for e in self.args:
                new_shape += e.shape
            return new_shape

    def _canonicalize(self):
        v = Variable('', self.shape)
        if shape.isscalar(v):
            return (v, [SOC(v, self.args)])
        else:
            return (v, [SOCProd(v, self.args)])

# register with the atom library
atom.atoms['norm'] = QC_norm
atom.atoms['norm2'] = QC_norm   # norm alias

