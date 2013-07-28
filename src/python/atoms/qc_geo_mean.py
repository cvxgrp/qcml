from qcml.properties.curvature import Constant, Convex, Concave, Affine
from qcml.properties.monotonicity import increasing, decreasing, nonmonotone
from qcml.properties.shape import Scalar, Vector, Matrix, isvector, ismatrix, isscalar
from qcml.properties.sign import ispositive, isnegative, Positive, Negative, Neither

from qcml.expressions.expression import Variable, Number
from qcml.expressions.qc_ast import Objective, Program, SOC, SOCProd

from utils import create_variable, annotate

""" This is the geo_mean atom.

        geo_mean(x,y) = sqrt(x * y)

    If either x or y is a vector, the atom is applied elementwise.

    It is a CONCAVE atom. It is DECREASING in the first argument, and
    DECREASING in the second argument.

    It returns a SCALAR expression if the both arguments are SCALAR.
    Otherwise, it returns a VECTOR expression (sized to match the largest
    arugment).

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
def attributes(x,y):
    sign = Positive()
    vexity = Concave() + increasing(x) + increasing(y)
    shape = x.shape + y.shape
    return (sign, vexity, shape)

@annotate('geo_mean')
def rewrite(p,x,y):
    """ Rewrite a quad_over_lin node

        p
            the parent node

        x, y
            the arguments
    """
    v = create_variable(p.shape)

    constraints = [
        SOCProd(x + y, [y - x, Number(2.0)*v]),
        y >= Number(0),
        x >= Number(0)
    ]

    return (v, constraints)

