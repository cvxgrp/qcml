from scoop.expression import Variable, Cone, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, \
    POSITIVE, NEGATIVE, CONVEX, CONCAVE, AFFINE
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
    v = Variable(create_varname(), x.shape) 
    definition = [v, Cone.SOC(v,x)]
    
    # lines = [ 
    #     "variable %s %s" % (v.name, str.lower(v.shape.shape_str)),
    #     "%s" % str(definition)
    # ]
    v.vexity, v.sign = vexity, POSITIVE      

    return (v, definition)  

