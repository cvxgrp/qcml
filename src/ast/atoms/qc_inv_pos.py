""" This is the inv_pos atom.

        inv_pos(x) = square_over_lin(1,x)

    It is a CONVEX atom. It is DECREASING in the first argument.

    It is always POSITIVE.

    It returns a VECTOR expressions.

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""

import atom
from utils import *

from qc_square_over_lin import QC_square_over_lin

class QC_inv_pos(QC_square_over_lin):
    def __init__(self, x):
        super(QC_inv_pos,self).__init__(Number(1), x)

    def __str__(self):
        return "inv_pos(%s)" % self.args[1]

# register with the atom library
atom.atoms['inv_pos'] = QC_inv_pos



