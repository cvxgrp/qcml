from scoop.expression import Expression, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, \
    POSITIVE, NEGATIVE, SCALAR, VECTOR, CONVEX, CONCAVE, AFFINE
from utils import create_varname, comment

# to prevent name clash with builtin, named with trailing '_'
@comment
def abs_(x):    
    # infer vexity from signed monotonicities
    vexity = CONVEX
    if ispositive(x): vexity |= increasing(x)
    elif isnegative(x): vexity |= decreasing(x)
    else: vexity |= nonmonotone(x)
    
    # the output is named differently, but is also an expression
    v = Expression(vexity, POSITIVE, x.shape, create_varname(), None) 
    
    lines = [ 
        "variable %s %s" % (v.name, str.lower(v.shape.shape_str)),
        "abs(%s) <= %s" % (x.name, v.name)
    ]       

    return (lines, v)  

