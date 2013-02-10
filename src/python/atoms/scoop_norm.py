from scoop.expression import Expression, Cone, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, \
    POSITIVE, NEGATIVE, SCALAR, VECTOR, CONVEX, CONCAVE, AFFINE
from utils import create_varname, comment

@comment
def norm(*args):    
    # infer vexity from signed monotonicities
    vexity = CONVEX
    if ispositive(x): vexity |= increasing(x)
    elif isnegative(x): vexity |= decreasing(x)
    else: vexity |= nonmonotone(x)
    
    arglist = ', '.join( map(lambda e: e.name, args) )
    # the output is named differently, but is also an expression
    v = Expression(vexity, POSITIVE, SCALAR, create_varname(), None)
    
    if len(args) == 1:
        # Cone.SOC(v, [x]), norm(x) <= v
        definition = Cone.SOC(v, args)
    else:
        # Cone.SOC(v,x,y,z,...), norm(x,y,z,...) <= v
        definition = Cone.SOC(v, *args)
    
    lines = [ 
        "variable %s %s" % (v.name, str.lower(v.shape.shape_str)),
        "%s" % str(definition)
    ] 
    
    return (lines, v)  
