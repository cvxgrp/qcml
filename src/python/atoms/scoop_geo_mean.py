from scoop.expression import Expression, Constant, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, \
    POSITIVE, NEGATIVE, SCALAR, VECTOR, CONVEX, CONCAVE, AFFINE
from utils import create_varname

# geo_mean(x,y) = sqrt(x .* y)
def geo_mean(x,y):
    # infer vexity from signed monotonicities
    vexity = CONCAVE | increasing(x) | increasing(y)

    # x is an (affine) Expression
    # the output is named differently, but is also an expression
    v = Expression(vexity, POSITIVE, x.shape + y.shape, create_varname(), None)
    
    definition = [
        -Constant(0.5)*x + Constant(0.5)*y,
        Constant(0.5)*x + Constant(0.5)*y,
        y >= Constant(0)
    ]
                
    # declare the expansion in "SCOOP"
    lines = filter(None, [
        "variable %s %s" % (v.name, str.lower(v.shape.shape_str)),
        "norm(%s, %s) <= %s" % (definition[0].name, v.name, definition[1].name),
        "%s" % str(definition[2])
    ])

    return (lines, v)
      
 
