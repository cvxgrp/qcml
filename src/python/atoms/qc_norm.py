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
        if len(self.args) == 1: return Scalar()
        else:
            new_shape = Scalar()
            for e in self.args:
                new_shape += e.shape
            return new_shape

    def _canonicalize(self):
        v = create_variable(self.shape)
        if shape.isscalar(v):
            return [SOC(v, self.args[0])]
        else:
            return [SOCProd(v, self.args)]

# register with the atom library
atom.atoms['norm'] = QC_norm

