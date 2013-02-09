from scoop.expression import Expression, Constant, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, \
    POSITIVE, NEGATIVE, SCALAR, VECTOR, CONVEX, CONCAVE, AFFINE
from utils import create_varname


def quad_over_lin(x,y):
    # infer vexity from signed monotonicities
    vexity = CONVEX
    if ispositive(x): vexity |= increasing(x) | decreasing(y)
    elif isnegative(x): vexity |= decreasing(x) | decreasing(y)
    else: vexity |= nonmonotone(x) | decreasing(y)

    # x is an (affine) Expression
    # the output is named differently, but is also an expression
    v = Expression(vexity, POSITIVE, y.shape, create_varname(), None)
    
    norm_arg = Constant(0.5)*y - Constant(0.5)*v
    rhs = Constant(0.5)*y + Constant(0.5)*v
                
    # declare the expansion in "SCOOP"
    if y.shape == SCALAR:
        lines = [
            "variable %s %s" % (v.name, str.lower(v.shape.shape_str)),
            "norm([ %s ; %s ]) <= %s" % (norm_arg.name, x.name, rhs.name),
            "%s >= 0" % y.name
        ]
    else:
        lines = [
            "variable %s %s" % (v.name, str.lower(v.shape.shape_str)),
            "norm( %s, %s) <= %s" % (norm_arg.name, x.name, rhs.name),
            "%s >= 0" % y.name
        ]

    return (lines, v)


  
  # -- quad_over_lin x y = x^Tx / y, if y is vector, it's x.^2 ./ y
  # scoop_quad_over_lin :: Expr -> Expr -> Rewriter Expr
  # scoop_quad_over_lin x y = do
  #   let v = case (sign x) of
  #             Positive -> Convex <&> increasing x <&> decreasing y
  #             Negative -> Convex <&> decreasing x <&> decreasing y
  #             otherwise -> Convex <&> nonmonotone x <&> decreasing y
  #   
  #   t <- newVar
  #   z0 <- newVar
  #   z1 <- newVar
  #   
  #   addLine $ concat ["0.5*", name y, " + 0.5*", t, " - ", z0, " == 0"]
  #   addLine $ concat ["0.5*", name y, " - 0.5*", t, " - ", z1, " == 0"]
  #   addLine $ (name y) ++ " >= 0"
  #   addLine $ concat ["norm([", z1, "; ", name x, "]) <= ", z0]