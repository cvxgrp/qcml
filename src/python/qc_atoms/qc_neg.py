from qcml.properties.curvature import Constant, Convex, Concave, Affine, increasing, decreasing, nonmonotone
from qcml.properties.shape import Scalar, Vector, Matrix, isvector, ismatrix, isscalar
from qcml.properties.sign import ispositive, isnegative, Positive, Negative, Neither

from qcml.expression.expression import Variable, Number
from qcml.expression.qc_ast import Objective, Program, SOC, SOCProd

import qc_max
from utils import annotate

""" This is the neg atom.

        neg(x) = max(-x,0)

    It is a CONVEX atom. It is INCREASING in the first argument.

    It returns a VECTOR expression.

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
def attributes(x):
    return qc_max.attributes(-x, Number(0.0))

@annotate('neg')
def rewrite(p,x):
    """ Rewrite a square node

        p
            the parent node

        x
            the argument
    """
    if isnegative(x):
        return (-x, [])
    else:
        return qc_max.rewrite(p,-x,Number(0.0))

