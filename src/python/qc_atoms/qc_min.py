from qcml.qc_ast import Scalar, Vector, Matrix, \
    ispositive, isnegative, \
    isvector, ismatrix, isscalar, \
    increasing, decreasing, nonmonotone, \
    Positive, Negative, Neither, \
    Convex, Concave, Affine, \
    Variable, Objective, Program, Number, \
    SOC, SOCProd
from utils import create_variable, annotate
import operator

#import scoop as s

""" This is the min atom.

        min(x) = minimum element of x

    It is a CONCAVE atom. It is INCREASING in the first argument.

    It returns a SCALAR expression. If multiple arguments are supplied, it
    compares them elementwise and returns a VECTOR expression.

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
def attributes(*args):
    if len(args) == 1:
        sign = args[0].sign
        shape = Scalar()
        vexity = Concave() + increasing(args[0])
    else:
        if any(isnegative(e) for e in args): sign = Negative()
        if all(ispositive(e) for e in args): sign = Positive()
        else: sign = Neither()
        shape = reduce(operator.add, map(lambda x: x.shape, args))
        vexity = reduce(operator.add, map(increasing, args), Concave())

    return (sign, vexity, shape)

@annotate('min')
def rewrite(p,*args):
    """ Rewrite a quad_over_lin node

        p
            the parent node

        x, y
            the arguments
    """
    v = create_variable(p.shape)

    constraints = map(lambda x: x >= v, args)

    return (v, constraints)
