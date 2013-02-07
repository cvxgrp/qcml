from scoop.expression \
import Variable, Expression, INCREASING, DECREASING, NONMONOTONE, \
    ispositive, isnegative, \
    POSITIVE, NEGATIVE, SCALAR, VECTOR, CONVEX, CONCAVE, AFFINE
# from scoop.ir.coeff import Coeff, Matrix, Eye, Zero, Ones
# import operator


def scoop_square(t):
    def square(x):
        # x is an (affine) Expression
        # the output is named differently, but is also an expression
        v = Expression(CONVEX, POSITIVE, x.shape, t)
        
        # declare the expansion in "SCOOP"
        lines = [
            "variable %s %s" % (t, str.lower(x.shape.shape_str)),
            "norm(0.5 - 0.5*%s, %s) <= 0.5 + 0.5*%s" % (v.name, x.name, v.name)
        ]
        
        if ispositive(x): mono = INCREASING
        elif isnegative(x): mono = DECREASING
        else: mono = NONMONOTONE
        
        return (lines, v, mono)
        
    return square
    
