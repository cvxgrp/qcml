from scoop.expression import Expression, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, \
    POSITIVE, NEGATIVE, SCALAR, VECTOR, CONVEX, CONCAVE, AFFINE
from utils import create_varname

# geo_mean(x,y) = sqrt(x .* y)
def geo_mean(x,y):
    # infer vexity from signed monotonicities
    vexity = CONCAVE | increasing(x) | increasing(y)

    # x is an (affine) Expression
    # the output is named differently, but is also an expression
    v = Expression(vexity, POSITIVE, x.shape + y.shape, create_varname())
                
    # declare the expansion in "SCOOP"
    lines = [
        "variable %s %s" % (v.name, str.lower(v.shape.shape_str)),
        "norm(-0.5*(%s) + 0.5*(%s), %s) <= 0.5*(%s) + 0.5*(%s)" % (x.name, y.name, v.name, x.name, y.name),
        "%s >= 0" % y.name
    ]

    return (lines, v)
      
  

  # 
  # -- pow_rat (x, p,q) = x^(p/q) for q <= p <= 4
  # scoop_pow_rat :: Expr -> Integer -> Integer -> Rewriter Expr
  # scoop_pow_rat x 4 3 = do
  #   let v = Convex <&> increasing x
  #   
  #   t <- newVar
  #   let y = Expr t v Positive
  #   e <- scoop_pow_rat y 3 4
  #   addLine $ concat [name x, " >= 0"]
  #   addLine $ concat [name e, " - ", name x, " >= 0"]
  #   
  #   return y
  # scoop_pow_rat x 4 2 = scoop_square x
  # scoop_pow_rat x 4 1 = do
  #   e <- scoop_square x
  #   scoop_square e
  # scoop_pow_rat x 3 2 = do
  #   e <- scoop_sqrt x
  #   scoop_quad_over_lin x e
  # scoop_pow_rat x 3 1 = do
  #   e <- scoop_square x
  #   scoop_quad_over_lin e x
  # scoop_pow_rat x 2 1 = scoop_square x
  # scoop_pow_rat x 1 2 = scoop_sqrt x
  # scoop_pow_rat x 1 3 = do
  #   let v = Concave <&> increasing x
  #   
  #   t <- newVar
  #   let y = Expr t v Positive
  #   e <- scoop_pow_rat y 3 1
  #   addLine $ concat [name x, " >= 0"]
  #   addLine $ concat [name x, " - ", name e, " >= 0"]
  #   
  #   return y
  # 
  # scoop_pow_rat x 2 3 = do
  #   let v = Concave <&> increasing x
  #   
  #   t <- newVar
  #   let y = Expr t v Positive
  #   
  #   e <- scoop_pow_rat y 3 2
  #   addLine $ concat [name x, " >= 0"]
  #   addLine $ concat [name x, " - ", name e, " >= 0"]
  #   
  #   return y
  # scoop_pow_rat x 1 4 = do
  #   e <- scoop_sqrt x
  #   scoop_sqrt e
  # scoop_pow_rat x 2 4 = scoop_sqrt x
  # scoop_pow_rat x 3 4 = do
  #   e <- scoop_sqrt x
  #   scoop_geo_mean x e
  # scoop_pow_rat x p q
  #   | p == q = return x
  #   | otherwise = fail $ "pow_rat: not implemented for p = " ++ show p ++ " and q = " ++ show q
  # 
  # --   -- sum_largest(x,k) <-- also not implemented (uses LP dual)
  # --   
