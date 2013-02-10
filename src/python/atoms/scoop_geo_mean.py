from scoop.expression import Expression, Constant, Cone, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, \
    POSITIVE, NEGATIVE, SCALAR, VECTOR, CONVEX, CONCAVE, AFFINE
from utils import create_varname, comment

# geo_mean(x,y) = sqrt(x .* y)
@comment
def geo_mean(x,y):
    # infer vexity from signed monotonicities
    vexity = CONCAVE | increasing(x) | increasing(y)

    # x is an (affine) Expression
    # the output is named differently, but is also an expression
    v = Expression(vexity, POSITIVE, x.shape + y.shape, create_varname(), None)
    
    definition = [
        Cone.SOC(Constant(0.5)*x + Constant(0.5)*y, -Constant(0.5)*x + Constant(0.5)*y, v),
        y >= Constant(0)
    ]
                
    # declare the expansion in "SCOOP"
    lines = filter(None, [
        "variable %s %s" % (v.name, str.lower(v.shape.shape_str)),
        "%s" % str(definition[0]),
        "%s" % str(definition[1])
    ])

    return (lines, v)
      
 
