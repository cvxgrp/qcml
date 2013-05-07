from scoop.qc_ast import Scalar, Vector, Matrix, \
    ispositive, isnegative, \
    isvector, ismatrix, isscalar, \
    increasing, decreasing, nonmonotone, \
    Positive, Negative, \
    Convex, Concave, Affine, \
    Variable, Objective, Program, Constant, \
    SOC, SOCProd
from utils import create_varname, annotate
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
    shape = reduce(operator.add, map(lambda x: x.shape, args), Scalar())
    
    v = Variable(create_varname(), shape)
    
    if isscalar(v):
        constraints = [SOC(v, [args[0]])]
    else:
        constraints = [SOCProd(v, args)]
    
    return (v, constraints)

@annotate('abs')
def abs_rewrite(p, x):
    v = Variable(create_varname(), x.shape)
    constraints = [SOCProd(v, [x])]
    
    return (v, constraints)