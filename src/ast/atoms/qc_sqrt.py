""" This is the sqrt atom.

        sqrt(x) = geo_mean(x,1)

    It is a CONCAVE atom. It is INCREASING in the first argument.

    It returns a VECTOR expressions.

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
import atom
from utils import *

from qc_geo_mean import QC_geo_mean



class QC_sqrt(QC_geo_mean):
    def __init__(self, x):
        super(QC_sqrt, self).__init__(x, Number(1))

    def __str__(self):
        return "sqrt(%s)" % self.args[0]

# register with the atom library
atom.atoms['sqrt'] = QC_sqrt


