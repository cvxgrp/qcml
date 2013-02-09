from scoop.expression import Expression, Constant, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, \
    POSITIVE, NEGATIVE, SCALAR, VECTOR, CONVEX, CONCAVE, AFFINE
from utils import create_varname, comment

@comment
def quad_over_lin(x,y):
    # infer vexity from signed monotonicities
    vexity = CONVEX
    if ispositive(x): vexity |= increasing(x) | decreasing(y)
    elif isnegative(x): vexity |= decreasing(x) | decreasing(y)
    else: vexity |= nonmonotone(x) | decreasing(y)

    # x is an (affine) Expression
    # the output is named differently, but is also an expression
    v = Expression(vexity, POSITIVE, y.shape, create_varname(), None)
    
    definition = [
        Constant(0.5)*y - Constant(0.5)*v,
        Constant(0.5)*y + Constant(0.5)*v,
        y >= Constant(0)
    ]
                
    # declare the expansion in "SCOOP"
    if y.shape == SCALAR:
        lines = filter(None, [
            "variable %s %s" % (v.name, str.lower(v.shape.shape_str)),
            "norm([ %s ; %s ]) <= %s" % (definition[0].name, x.name, definition[1].name),
            "%s" % str(definition[2])
        ])
    else:
        lines = filter(None, [
            "variable %s %s" % (v.name, str.lower(v.shape.shape_str)),
            "norm( %s, %s ) <= %s" % (definition[0].name, x.name, definition[1].name),
            "%s" % str(definition[2])
        ])

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