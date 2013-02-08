from scoop.expression import Constant
from scoop_qol import quad_over_lin

def square(x):
    return quad_over_lin(x, Constant(1.0))
    # # infer vexity from signed monotonicities
    # vexity = CONVEX
    # if ispositive(x): vexity |= increasing(x)
    # elif isnegative(x): vexity |= decreasing(x)
    # else: vexity |= nonmonotone(x)
    # 
    # # x is an (affine) Expression
    # # the output is named differently, but is also an expression
    # v = Expression(vexity, POSITIVE, x.shape, create_varname())
    #             
    # # declare the expansion in "SCOOP"
    # lines = [
    #     "variable %s %s" % (v.name, str.lower(x.shape.shape_str)),
    #     "norm(0.5 - 0.5*%s, %s) <= 0.5 + 0.5*%s" % (v.name, x.name, v.name)
    # ]
    # 
    # return (lines, v)

