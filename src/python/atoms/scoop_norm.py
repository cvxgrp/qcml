from scoop.expression import Expression, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, \
    POSITIVE, NEGATIVE, SCALAR, VECTOR, CONVEX, CONCAVE, AFFINE
from utils import create_varname


# to prevent name clash with builtin, named with trailing '_'
def norm(*args):    
    # infer vexity from signed monotonicities
    vexity = CONVEX
    if ispositive(x): vexity |= increasing(x)
    elif isnegative(x): vexity |= decreasing(x)
    else: vexity |= nonmonotone(x)
    
    arglist = ', '.join( map(lambda e: e.name, args) )
    # the output is named differently, but is also an expression
    v = Expression(vexity, POSITIVE, SCALAR, create_varname())
    
    lines = [ 
        "variable %s %s" % (v.name, str.lower(v.shape.shape_str)),
        "norm(%s) <= %s" % (arglist, v.name)
    ] 
    
    return (lines, v)  
