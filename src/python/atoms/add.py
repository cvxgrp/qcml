from scoop.scoop_expression \
import Variable, Parameter, Constant, Expression, \
    convex, concave, affine, increasing, decreasing, nonmonotone, \
    POSITIVE, NEGATIVE, UNKNOWN, \
    SCALAR, VECTOR, MATRIX

# some atoms
@affine
def op_add(self,x,y):
    if isinstance(x,Constant) and isinstance(y,Constant):
        return Constant(x.value + y.value)
    
    # vexity from monotonicity
    vex = increasing(x) | increasing(y)
    # signs from args (uses dictionary)
    sign = {
        (POSITIVE,POSITIVE): POSITIVE, 
        (NEGATIVE,NEGATIVE): NEGATIVE
    }.get((x.sign, y.sign), UNKNOWN)
    
    # figure out the shape
    if x.shape is MATRIX or y.shape is MATRIX:
        raise Exception("Cannot add two parameter matrices %(x)s and %(y)s." % locals())
    shape = {
        (SCALAR,SCALAR): SCALAR
    }.get((x.shape, y.shape), VECTOR)
    

    
    return Expression(vex,sign,shape)
    
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

