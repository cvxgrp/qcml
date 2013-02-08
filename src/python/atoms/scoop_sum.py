from scoop.expression import Expression, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, \
    POSITIVE, NEGATIVE, SCALAR, VECTOR, CONVEX, CONCAVE, AFFINE
from utils import create_varname
import operator


# to prevent name clash with builtin, named with trailing '_'
def sum_(*args):    
    # infer vexity from signed monotonicities
    vexity = AFFINE | reduce(operator.or_, map(increasing, args))
    
    # the output is named differently, but is also an expression
    # determine the shape of the output (scalar if vector input, vector if 
    # list input)
    if len(args) == 1:
        v = Expression(vexity, args[0].sign, SCALAR, "sum(%s)" % args[0].name)        
    elif len(args) > 1:
        sign = reduce(operator.add, map(lambda x: x.sign, args))
        shape = reduce(operator.add, map(lambda x: x.shape, args))
        
        arglist = ' + '.join( map(lambda e: e.name, args) )
        v = Expression(vexity, sign, shape, "(%s)" % arglist )
    else:
        raise Exception("'sum' cannot be called with zero arguments.")

      
    return ([], v)  
