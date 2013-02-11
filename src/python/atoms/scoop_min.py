from scoop.expression import Variable, Constant, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, \
    POSITIVE, NEGATIVE, UNKNOWN, \
    SCALAR, VECTOR, CONVEX, CONCAVE, AFFINE
from utils import create_varname, comment
import operator

# to prevent name clash with builtin, named with trailing '_'
@comment
def min_(*args):    
    # infer vexity from signed monotonicities
    vexity = CONCAVE | reduce(operator.or_, map(increasing, args))
    
    # the output is named differently, but is also an expression
    # determine the shape of the output (scalar if vector input, vector if 
    # list input)
    if len(args) == 1:
        # set the vexity to affine temporarily, so we can form v <= x expr
        v = Variable(create_varname(), SCALAR)
        sign = args[0].sign      
    elif len(args) > 1:
        if any(isnegative(e) for e in args): sign = NEGATIVE
        if all(ispositive(e) for e in args): sign = POSITIVE
        else: sign = UNKNOWN
        shape = reduce(operator.add, map(lambda x: x.shape, args))
        
        # set the vexity to affine temporarily, so we can form v >= x expr
        v = Variable(create_varname(), shape)
    else:
        raise Exception("'min' cannot be called with zero arguments.")

    # declare the expansion in "SCOOP"
    constraints = map(lambda x: x >= v, args)
    # lines = [ 
    #     "variable %s %s" % (v.name, str.lower(v.shape.shape_str)) 
    # ] + filter(None, map(lambda x: "%s" % str(x), constraints ))
    #             
    v.vexity, v.sign = vexity, sign
    return (v, [v] + constraints) 
    
