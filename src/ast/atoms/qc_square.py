""" This is the square atom.

        square(x) = square_over_lin(x,1)

    If y is a vector, it computes the division elementwise.

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

from qc_square_over_lin import QC_square_over_lin

class QC_square(QC_square_over_lin):
    def __init__(self, x):
        super(QC_square,self).__init__(x, Number(1))

    def __str__(self):
        return "square(%s)" % self.args[0]

# register with the atom library
atom.atoms['square'] = QC_square

