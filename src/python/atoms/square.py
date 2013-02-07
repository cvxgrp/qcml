from scoop.expression \
import Expression, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, \
    POSITIVE, NEGATIVE, SCALAR, VECTOR, CONVEX, CONCAVE, AFFINE
# from scoop.ir.coeff import Coeff, Matrix, Eye, Zero, Ones
# import operator


def scoop_square(env):
    def square(x):
        # infer vexity from signed monotonicities
        vexity = CONVEX
        if ispositive(x): vexity |= increasing(x)
        elif isnegative(x): vexity |= decreasing(x)
        else: vexity |= nonmonotone(x)
        
        expr_string = 'square(%s)' % x.name
        
        # this is all scaffolding
        # maybe we can wrap it?
        v = env.lookup.get(expr_string, None)
        if v:
            return ([], v)
        else:
            # x is an (affine) Expression
            # the output is named differently, but is also an expression
            v = Expression(vexity, POSITIVE, x.shape, env.create_varname())
        
            env.lookup[expr_string] = v
        
            # declare the expansion in "SCOOP"
            lines = [
                "",
                "# '%s' replaces '%s'" % (v.name, expr_string),
                "variable %s %s" % (v.name, str.lower(x.shape.shape_str)),
                "norm(0.5 - 0.5*%s, %s) <= 0.5 + 0.5*%s" % (v.name, x.name, v.name)
            ]
    
            return (lines, v)
        
    return square
    
