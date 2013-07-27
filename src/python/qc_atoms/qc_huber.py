import qc_square as square

from qcml.properties.curvature import Constant, Convex, Concave, Affine, increasing, decreasing, nonmonotone
from qcml.properties.shape import Scalar, Vector, Matrix, isvector, ismatrix, isscalar
from qcml.properties.sign import ispositive, isnegative, Positive, Negative, Neither

from qcml.expression.expression import Variable, Number
from qcml.expression.qc_ast import Objective, Program, SOC, SOCProd

from utils import create_variable, annotate

""" This is the huber atom.

        huber(x) = minimize(w.^2 + 2*v) s.t. (abs(x) <= w + v; w<=1; v>=0)

    If x is a vector, it computes the elementwise huber.

    It is a CONVEX atom. It is NONMONOTONE in the first argument.

    If the first argument is POSITIVE, it is INCREASING in the first argument.
    If the first argument is NEGATIVE, it is DECRASING in the first argument.

    It returns a VECTOR expression.

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
def attributes(x):
    return square.attributes(x)

@annotate('huber')
def rewrite(p,x):
    """ Rewrite a quad_over_lin node

        p
            the parent node

        x, y
            the arguments
    """
    w = create_variable(p.shape)
    v = create_variable(p.shape)

    v1,d1 = abs_rewrite(p, x)
    v2,d2 = square.rewrite(p, w)

    constraints = d1 + d2 + [
        v1 <= w + v,
        w <= Number(1),
        v >= Number(0)
    ]

    return (v2 + Number(2)*v, constraints)


