from scoop.expression import Constant
from scoop_qol import quad_over_lin


def inv_pos(x): 
    return quad_over_lin(Constant(1.0), x)   
    # # infer vexity from signed monotonicities
    # vexity = CONVEX | decreasing(x)
    # 
    # # the output is named differently, but is also an expression
    # v = Expression(vexity, POSITIVE, x.shape, create_varname())
    #             
    # # declare the expansion in "SCOOP"
    # lines = [
    #     "variable %s scalar" % v.name,
    #     "norm( 0.5*%s - 0.5*%s, 1.0 ) <= 0.5*%s + 0.5*%s" % (x.name, v.name, x.name, v.name),
    #     "%s >= 0" % x.name
    # ]
    # 
    # return (lines, v)  
  