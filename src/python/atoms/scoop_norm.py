from scoop.expression import Variable, Cone, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, Scalar, \
    POSITIVE, NEGATIVE, CONVEX, CONCAVE, AFFINE
from utils import create_varname, comment
import operator

@comment
def norm(*args):    
    # infer vexity from signed monotonicities
    vexity = CONVEX
    if all(ispositive(e) for e in args): vexity |= reduce(operator.or_, map(increasing, args))
    elif all(isnegative(e) for e in args): vexity |= reduce(operator.or_, map(decreasing, args))
    else: vexity |= reduce(operator.or_, map(nonmonotone, args))
    
    if len(args) == 1:
        # create a new variable
        v = Variable(create_varname(), Scalar())
        
        # Cone.SOC(v, [x]), norm(x) <= v
        definition = [v, Cone.SOC(v, [args[0]])]
    else:
        # create a new variable (assumes all args have the same shape)
        v = Variable(create_varname(), args[0].shape)
        
        # Cone.SOC(v,x,y,z,...), norm(x,y,z,...) <= v
        definition = [v, Cone.SOC(v, *args)]
    
    # lines = [ 
    #     "variable %s %s" % (v.name, str.lower(v.shape.shape_str)),
    #     "%s" % str(definition)
    # ] 
    
    v.vexity, v.sign = vexity, POSITIVE
    return (v, definition)  
