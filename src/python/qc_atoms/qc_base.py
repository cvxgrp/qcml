from qcml.properties.curvature import Constant, Convex, Concave, Affine, increasing, decreasing, nonmonotone
from qcml.properties.shape import Scalar, Vector, Matrix, isvector, ismatrix, isscalar
from qcml.properties.sign import ispositive, isnegative, Positive, Negative, Neither

from qcml.expression.expression import Variable, Number
from qcml.expression.qc_ast import Objective, Program, SOC, SOCProd

from utils import create_variable, annotate
import operator

""" These define the attributes of the *basic* atoms, norm and abs, along with
    their rewrite rules
"""
def norm(args):
    sign = Positive()
    if all(ispositive(e) for e in args): vexity = reduce(operator.add, map(increasing, args), Convex())
    elif all(isnegative(e) for e in args): vexity = reduce(operator.add, map(decreasing, args), Convex())
    else: vexity = reduce(operator.add, map(nonmonotone, args), Convex())

    if len(args) == 1:
        shape = Scalar()
    else:
        shape = reduce(operator.add, map(lambda x: x.shape, args), Scalar())

    return (sign, vexity, shape)

def abs_(x):
    sign = Positive()
    if ispositive(x): vexity = Convex() + increasing(x)
    elif isnegative(x): vexity = Convex() + decreasing(x)
    else: vexity = Convex() + nonmonotone(x)
    shape = x.shape

    return (sign, vexity, shape)

""" Rewrite rules
"""
@annotate('norm')
def norm_rewrite(p, args):
    if len(args) == 1:
        shape = Scalar()
    else:
        shape = reduce(operator.add, map(lambda x: x.shape, args), Scalar())

    v = create_variable(shape)

    if isscalar(v):
        constraints = [SOC(v, [args[0]])]
    else:
        constraints = [SOCProd(v, args)]

    return (v, constraints)

@annotate('abs')
# TODO: rewrite using linear constraints instead of SOC constraints
# or have SOCProd realize that it can just create two linear constraints
def abs_rewrite(p, x):
    v = create_variable(x.shape)
    constraints = [SOCProd(v, [x])]

    return (v, constraints)