import atom
from utils import *

from qc_max import QC_max

""" This is the min atom.

        min(x) = minimum element of x

    It is a CONCAVE atom. It is INCREASING in the first argument.

    It returns a SCALAR expression. If multiple arguments are supplied, it
    compares them elementwise and returns a VECTOR expression.

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
def QC_min(*args):
    return -QC_max(*[-x for x in args])

atom.atoms['min'] = QC_min
