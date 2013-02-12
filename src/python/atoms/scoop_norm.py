from scoop.expression import Variable, Cone, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, Scalar, \
    POSITIVE, NEGATIVE, CONVEX, CONCAVE, AFFINE
from utils import create_varname, comment

@comment
def norm(*args):    
    # infer vexity from signed monotonicities
    vexity = CONVEX
    if ispositive(x): vexity |= increasing(x)
    elif isnegative(x): vexity |= decreasing(x)
    else: vexity |= nonmonotone(x)
    
    # create a new variable
    v = Variable(create_varname(), Scalar())
    
    if len(args) == 1:
        # Cone.SOC(v, [x]), norm(x) <= v
        definition = [v, Cone.SOC(v, args)]
    else:
        # Cone.SOC(v,x,y,z,...), norm(x,y,z,...) <= v
        definition = [v, Cone.SOC(v, *args)]
    
    # lines = [ 
    #     "variable %s %s" % (v.name, str.lower(v.shape.shape_str)),
    #     "%s" % str(definition)
    # ] 
    
    v.vexity, v.sign = vexity, POSITIVE
    
    return (v, definition)  
