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
    shape = None
    if isscalar(y.shape):
        shape = x.shape
    elif isscalar(x.shape):
        shape = y.shape
    else:
        raise SyntaxError("Cannot use quad over lin with x, y both vector arguments.")
        
    v = Variable(create_varname(), shape)
                
    # declare the expansion in "SCOOP"
    if isscalar(shape):
        definition = [
            v,  # declare the variable
            # norm([(1/2)(y-v); x]) <= (1/2)(y + v)
            # norm([y-v; 2x]) <= y+v
            Cone.SOC(y + v, [y - v, Constant(2.0)*x]),
            y >= Constant(0)
        ]

    else:
        definition = [
            v, # declare the variable
            # norm([(1/2)(y-v); x]) <= (1/2)(y + v)
            # norm([y-v; 2x]) <= y+v
            Cone.SOC(y + v, y - v, Constant(2.0)*x),
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