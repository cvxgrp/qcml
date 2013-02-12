from scoop.expression import Variable, Constant, Cone, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, isscalar, \
    POSITIVE, NEGATIVE, CONVEX, CONCAVE, AFFINE 
from utils import create_varname, comment

@comment
def quad_over_lin(x,y):
    # infer vexity from signed monotonicities
    vexity = CONVEX
    if ispositive(x): vexity |= increasing(x) | decreasing(y)
    elif isnegative(x): vexity |= decreasing(x) | decreasing(y)
    else: vexity |= nonmonotone(x) | decreasing(y)

    # declare a new variable
    v = Variable(create_varname(), y.shape)
                
    # declare the expansion in "SCOOP"
    if isscalar(y.shape):
        definition = [
            v,  # declare the variable
            # norm([(1/2)(t-v); x]) <= (1/2)(y + v)
            Cone.SOC(Constant(0.5)*y + Constant(0.5)*v, [Constant(0.5)*y - Constant(0.5)*v, x]),
            y >= Constant(0)
        ]

    else:
        definition = [
            v, # declare the variable
            # norm([(1/2)(t-v); x]) <= (1/2)(y + v)
            Cone.SOC(Constant(0.5)*y + Constant(0.5)*v, Constant(0.5)*y - Constant(0.5)*v, x),
            y >= Constant(0)
        ]
        
    # set vexity and signs (this is for vexity and sign inference in remaining expression)
    v.vexity, v.sign = vexity, POSITIVE
    # v.sign = POSITIVE

    return (v, definition)


  
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