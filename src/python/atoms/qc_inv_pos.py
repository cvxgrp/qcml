import qc_square_over_lin as sol

from qcml.properties.curvature import Constant, Convex, Concave, Affine
from qcml.properties.monotonicity import increasing, decreasing, nonmonotone
from qcml.properties.shape import Scalar, Vector, Matrix, isvector, ismatrix, isscalar
from qcml.properties.sign import ispositive, isnegative, Positive, Negative, Neither

from qcml.expressions.expression import Variable, Number
from qcml.expressions.qc_ast import Objective, Program, SOC, SOCProd

from utils import annotate

""" This is the inv_pos atom.

        inv_pos(x) = square_over_lin(1,x)

    It is a CONVEX atom. It is DECREASING in the first argument.

    It is always POSITIVE.

    It returns a VECTOR expressions.

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
def attributes(x):
    return sol.attributes(Number(1.0), x)

@annotate('inv_pos')
def rewrite(p,x):
    """ Rewrite an inv_pos node

        p
            the parent node

        x
            the argument
    """
    return sol.rewrite(p,Number(1.0),x)


