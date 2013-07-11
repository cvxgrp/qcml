from qcml.qc_ast import Scalar, Vector, Matrix, \
    ispositive, isnegative, \
    isvector, ismatrix, isscalar, \
    increasing, decreasing, nonmonotone, \
    Positive, Negative, \
    Convex, Concave, Affine, \
    Variable, Objective, Program, Constant, \
    SOC, SOCProd
from utils import create_varname, annotate

#import scoop as s

""" This is the quad_over_lin atom.

        quad_over_lin(x,y) = (x^Tx) ./ y

    If y is a vector, it computes the division elementwise.

    It is a CONVEX atom. It is NONMONOTONE in the first argument, and
    DECREASING in the second argument.

    If the first argument is POSITIVE, it is INCREASING in the first argument.
    If the first argument is NEGATIVE, it is DECRASING in the first argument.

    It returns a SCALAR expression if the second argument is SCALAR.
    Otherwise, it returns a VECTOR expression (sized to match the second
    arugment).

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
def attributes(x,y):
    sign = Positive()
    vexity = Convex()
    if ispositive(x): vexity += increasing(x) + decreasing(y)
    elif isnegative(x): vexity += decreasing(x) + decreasing(y)
    else: vexity += nonmonotone(x) + decreasing(y)

    if isscalar(y):
        shape = Scalar()
    else:
        raise TypeError("Cannot use quad_over_lin with vector y arguments. Perhaps you meant 'square_over_lin'?")

    return (sign, vexity, shape)

@annotate('quad_over_lin')
def rewrite(p,x,y):
    """ Rewrite a quad_over_lin node

        p
            the parent node

        x, y
            the arguments
    """
    v = Variable(create_varname(), p.shape)

    constraints = [
        SOC(y + v, [y - v, Constant(2.0)*x]),
        y >= Constant(0)
    ]

    return (v, constraints)

    # v = Variable(create_varname(), shape)
    #
    #     # declare the expansion in "SCOOP"
    #     if isscalar(shape):
    #         definition = [
    #             v,  # declare the variable
    #             # norm([(1/2)(y-v); x]) <= (1/2)(y + v)
    #             # norm([y-v; 2x]) <= y+v
    #             Cone.SOC(y + v, [y - v, Constant(2.0)*x]),
    #             y >= Constant(0)
    #         ]
    #
    #     else:
    #         definition = [
    #             v, # declare the variable
    #             # norm([(1/2)(y-v); x]) <= (1/2)(y + v)
    #             # norm([y-v; 2x]) <= y+v
    #             Cone.SOC(y + v, y - v, Constant(2.0)*x),
    #             y >= Constant(0)
    #         ]
    #


