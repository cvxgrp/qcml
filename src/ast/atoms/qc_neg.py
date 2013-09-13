import atom
from utils import *

from qc_max import QC_max

""" This is the neg atom.

        neg(x) = max(-x,0)

    It is a CONVEX atom. It is INCREASING in the first argument.

    It returns a VECTOR expressions.

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""

class QC_neg(QC_max):
    def __init__(self, x):
        super(QC_neg,self).__init__(-x, Number(0))

    def __str__(self):
        return "neg(%s)" % self.args[0]

# register with the atom library
atom.atoms['neg'] = QC_neg
