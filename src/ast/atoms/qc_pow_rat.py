import atom
from utils import *

from qc_square import QC_square
from qc_square_over_lin import QC_square_over_lin
from qc_sqrt import QC_sqrt
from qc_geo_mean import QC_geo_mean
from qc_inv_pos import QC_inv_pos

def three_fourths(x):
    return QC_geo_mean(x, QC_sqrt(x))

def three_halves(x):
    return QC_square_over_lin(x, QC_sqrt(x))

def cube(x):
    return QC_square_over_lin(QC_square(x), x)

class one_third(QC_sqrt):
    def __init__(self, x):
        super(one_third, self).__init__(x)

    def __str__(self): return "pow_rat(%s, 1, 3)" % self.args[0]

    def _canonicalize(self):
        v = Variable('', self.shape)
        x = self.args[0]
        constraints = [
            cube(v) <= x
        ]
        return (v, constraints)

class two_thirds(QC_sqrt):
    def __init__(self, x):
        super(two_thirds, self).__init__(x)

    def __str__(self): return "pow_rat(%s, 2, 3)" % self.args[0]

    def _canonicalize(self):
        v = Variable('', self.shape)
        x = self.args[0]
        constraints = [
            three_halves(v) <= x
        ]
        return (v, constraints)

class four_thirds(QC_square):
    def __init__(self, x):
        super(four_thirds, self).__init__(x)

    def __str__(self): return "pow_rat(%s, 4, 3)" % self.args[0]

    def _monotonicity(self):
        return [monotonicity.increasing]

    def _canonicalize(self):
        v = Variable('', self.shape)
        x = self.args[0]
        constraints = [
            x <= three_fourths(v)
        ]
        return (v, constraints)

# only valid entries for pow_rat
pow_rat_func = {
    (1,1): lambda x: x,
    (1,2): QC_sqrt,
    (1,3): one_third,
    (1,4): lambda x: QC_sqrt(QC_sqrt(x)),
    (2,1): QC_square,
    (2,2): lambda x: x,
    (2,3): two_thirds,
    (2,4): QC_sqrt,
    (3,1): cube,
    (3,2): three_halves,
    (3,3): lambda x: x,
    (3,4): three_fourths,
    (4,1): lambda x: QC_square(QC_square(x)),
    (4,2): QC_square,
    (4,3): four_thirds,
    (4,4): lambda x: x
}

def QC_pow_rat(x,p,q):
    if isnumber(p) and isnumber(q):
        try:
            result = pow_rat_func[(p.value, q.value)](x)
        except KeyError:
            raise TypeError("Nonexistent implementation for %s^(%s/%s)" % (x.value, p.value, q.value))
        return result
    else:
        raise TypeError("Cannot use non-constant arguments p = %s, q = %s" % (str(p), str(q)))

atom.atoms['pow_rat'] = QC_pow_rat
