from scoop.expression \
import Expression, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, \
    POSITIVE, NEGATIVE, SCALAR, VECTOR, CONVEX, CONCAVE, AFFINE
# from scoop.ir.coeff import Coeff, Matrix, Eye, Zero, Ones
# import operator


def scoop_square(t):
    def square(x):
        # infer vexity from signed monotonicities
        vexity = CONVEX
        if ispositive(x): vexity |= increasing(x)
        elif isnegative(x): vexity |= decreasing(x)
        else: vexity |= nonmonotone(x)
        
        # x is an (affine) Expression
        # the output is named differently, but is also an expression
        v = Expression(vexity, POSITIVE, x.shape, t)
        
        # declare the expansion in "SCOOP"
        lines = [
            "",
            "# '%s' replaces 'square(%s)'" % (v.name, x.name),
            "variable %s %s" % (t, str.lower(x.shape.shape_str)),
            "norm(0.5 - 0.5*%s, %s) <= 0.5 + 0.5*%s" % (v.name, x.name, v.name)
        ]
    
        return (lines, v)
        
    return square
    
