from scoop.expression \
import Variable, Parameter, Constant, Expression, \
    convex, concave, affine, increasing, decreasing, nonmonotone, \
    fold_with, expand_all_args,\
    POSITIVE, NEGATIVE, UNKNOWN, \
    SCALAR, VECTOR, MATRIX
from scoop.ir.coeff import Coeff, Matrix, Eye, Zero, Ones
import operator

@fold_with(operator.add)
@expand_all_args
@affine
def op_add(self,x,y):
    # vexity from monotonicity
    vex = increasing(x) | increasing(y)
    # signs from args (uses dictionary)
    sign = {
        (POSITIVE,POSITIVE): POSITIVE, 
        (NEGATIVE,NEGATIVE): NEGATIVE
    }.get((x.sign, y.sign), UNKNOWN)
    
    # figure out the shape
    shape = {
        (SCALAR,SCALAR): SCALAR
    }.get((x.shape, y.shape), VECTOR)
    
    # create a new variable
    v = self.new_var(shape)
    
    if shape is SCALAR: m = 1
    else: m = v.name
    
    # equate the dimensions and start building the IR
    row = []
    if x.shape is SCALAR:
        row.append( (Ones(m), x) )        
    else:
        row.append( (Eye(m), x) )
        self.dimensions.equate(v.name, x.name)
    if y.shape is SCALAR:
        row.append( (Ones(m), y) )
    else:
        row.append( (Eye(m), y) )
        self.dimensions.equate(v.name, y.name)
    
    row += [ (Eye(m,-1), v), (Zero(m),)]
    
    # add row to IR        
    self.affine.append( row )
    
    return Expression(v.name, "%s + %s" % (x.description, y.description), vex,sign,shape)
    
    # primitiveAdd :: Expr -> Expr -> Rewriter Expr
    # primitiveAdd x y = do
    #   t <- newVar
    # 
    #   let v = Affine <&> increasing x <&> increasing y
    #   let s = case (sign x, sign y) of
    #             (Positive, Positive) -> Positive
    #             (Negative, Negative) -> Negative
    #             otherwise -> Unknown
    #   -- definition of plus
    #   addLine $ concat [t, " == ", (name x), " + ", (name y)]
    #   return $ Expr t v s

