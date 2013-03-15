from scoop.expression import Variable, Constant, Cone, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, \
    POSITIVE, NEGATIVE, CONVEX, CONCAVE, AFFINE
from utils import create_varname, comment

# geo_mean(x,y) = sqrt(x .* y)
@comment
def geo_mean(x,y):
    # infer vexity from signed monotonicities
    vexity = CONCAVE | increasing(x) | increasing(y)

    # x is an (affine) Expression
    # the output is named differently, but is also an expression
    v = Variable(create_varname(), x.shape + y.shape)
    
    # declare definiton
    constraints = [
        v, # declare variable
        Cone.SOC(x + y, y - x, Constant(2.0)*v),
        y >= Constant(0)
    ]
                
    # declare the expansion in "SCOOP"
    # lines = filter(None, [
    #     "variable %s %s" % (v.name, str.lower(v.shape.shape_str)),
    #     "%s" % str(definition[0]),
    #     "%s" % str(definition[1])
    # ])
    
    v.vexity, v.sign = vexity, POSITIVE

    return (v, constraints)
      
 
