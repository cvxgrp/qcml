from scoop.expression import Variable, Constant, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, \
    POSITIVE, NEGATIVE, SCALAR, VECTOR, CONVEX, CONCAVE, AFFINE
from utils import create_varname, comment
import operator


# to prevent name clash with builtin, named with trailing '_'
@comment
def sum_(*args):    
    # infer vexity from signed monotonicities
    vexity = AFFINE | reduce(operator.or_, map(increasing, args))
    
    # determine if it's a row sum or a column sum
    if len(args) == 1:
        # column sum
        v = Variable("sum(%s)" % args[0].name, SCALAR)
        v.vexity, v.sign = vexity, args[0].sign
    elif len(args) > 1:
        # row sum
        sign = reduce(operator.add, map(lambda x: x.sign, args))
        shape = reduce(operator.add, map(lambda x: x.shape, args))
        
        v = sum(args, Constant(0))
    else:
        raise Exception("'sum' cannot be called with zero arguments.")


    return (v, [])
