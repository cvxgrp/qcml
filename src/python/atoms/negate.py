from scoop.scoop_expression \
import Variable, Parameter, Constant, Expression, \
  convex, concave, affine, increasing, decreasing, nonmonotone, \
  fold_with, expand_all_args, \
  POSITIVE, NEGATIVE, UNKNOWN, \
  SCALAR, VECTOR, MATRIX
from scoop.ir.coeff import Coeff, Scalar, Matrix, Vector, Eye, Zero, Ones
from scoop.ir.dimension import Row, Col

import operator

@fold_with(operator.neg)
@expand_all_args
@affine
def op_neg(self,x):    
    # vexity from monotonicity
    vex = decreasing(x)
    
    # signs from args (uses dictionary)
    sign = {
      POSITIVE: NEGATIVE, 
      NEGATIVE: POSITIVE
    }.get(x.sign, UNKNOWN)
    
    # figure out the shape
    shape = x.shape
    
    # create a new variable
    v = self.new_var(shape)
    
    if shape is SCALAR: m = 1
    else: m = v.name
    
    # equate the dimensions and start building the IR
    self.dimensions.equate(x.name, v.name)
    row = [ (Eye(m,-1), x), (Eye(m,-1), v), (Zero(m),)]
    
    # add row to IR        
    self.affine.append( row )
            
    return Expression(v.name, "-(%s)" % x.description, vex,sign,shape)
    
  
  # primitiveNegate :: Expr -> Rewriter Expr
  #   primitiveNegate x = do
  #     t <- newVar
  # 
  #     let v = Affine <&> decreasing x
  #     let s = case (sign x) of
  #               Positive -> Negative
  #               Negative -> Positive
  #               otherwise -> Unknown
  # 
  #     addLine $ concat [t, " == -", (name x)]
  #     return $ Expr t v s
  # 
  