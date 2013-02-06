from scoop.scoop_expression \
import Variable, Parameter, Constant, Expression, \
  convex, concave, affine, increasing, decreasing, nonmonotone, \
  fold_with, expand_all_args, \
  POSITIVE, NEGATIVE, UNKNOWN, \
  SCALAR, VECTOR, MATRIX
from scoop.ir.coeff import Coeff, Scalar, Matrix, Vector, Eye, Zero, Ones
from scoop.ir.dimension import Row, Col

import operator

@fold_with(operator.mul)
@affine
def op_mult(self,x,y):
    if not isinstance(x, Parameter) and not isinstance(x, Constant):
        raise Exception("Cannot multiply non-parameter %s with expression %s." % (x.name, y.description))
    y = self.expand(y)

    
    # vexity from monotonicity
    vex = {
      POSITIVE: increasing(y), 
      NEGATIVE: decreasing(y)
    }.get(x.sign, nonmonotone(y))
    
    # signs from args (uses dictionary)
    sign = {
      (POSITIVE,POSITIVE): POSITIVE, 
      (NEGATIVE,NEGATIVE): POSITIVE,
      (POSITIVE,NEGATIVE): NEGATIVE,
      (NEGATIVE,POSITIVE): NEGATIVE
    }.get((x.sign, y.sign), UNKNOWN)
    
    # figure out the shape
    shape = {
      (SCALAR,SCALAR): SCALAR,
      (SCALAR,VECTOR): VECTOR,
      (VECTOR,SCALAR): VECTOR,
      (MATRIX,VECTOR): VECTOR
    }.get((x.shape, y.shape), None)
    if shape is None:
        raise Exception("Cannot multiply %s and %s." % (Parameter.shape_names[x.shape], Variable.shape_names[y.shape]))
    
    # create a new variable
    v = self.new_var(shape)
    
    if shape is SCALAR: m = 1
    else: m = v.name
    
    # equate the dimensions and start building the IR
    row = []
    if y.shape is SCALAR:
        if x.shape is VECTOR:
            row.append( (Vector(x), y) )
        else:
            row.append( (Scalar(x), y) )       
    else:
        if x.shape is MATRIX:
            row.append( (Matrix(x), y) )
        else:
            row.append( (Eye(m,x), y) )
            self.dimensions.equate(y.name, v.name)
    
    row += [ (Eye(m,-1), v), (Zero(m),)]
    
    # add row to IR        
    self.affine.append( row )
    
    # if parameter, add to list and also make the size concrete
    if isinstance(x,Parameter): self.paramlist[x.name] = x
    if x.shape is MATRIX:
        self.equiv[v.name] = Row(x) # LHS of multiply, TODO: depends on if transposed
        self.equiv[y.name] = Col(x) # RHS of multiply
    elif x.shape is VECTOR:
        self.equiv[v.name] = Row(x) # TODO: depends on if transposed
        
    
    return Expression(v.name, "%s*(%s)" % (x.description, y.description), vex,sign,shape)
    
  #   primitiveMultiply :: Param -> Expr -> Rewriter Expr
  #   primitiveMultiply p x = do
  #     t <- newVar
  # 
  #     let v = case (sign p) of
  #               Positive -> Affine <&> increasing x
  #               Negative -> Affine <&> decreasing x
  #               otherwise -> Affine <&> nonmonotone x
  #     let s = case (sign p, sign x) of
  #               (Positive, Positive) -> Positive
  #               (Negative, Negative) -> Positive
  #               (Positive, Negative) -> Negative
  #               (Negative, Positive) -> Negative
  #               otherwise -> Unknown
  # 
  #     addLine $ concat [t, " == ", (name p), "*", (name x)]
  #     return $ Expr t v s
  