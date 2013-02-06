from scoop.scoop_expression \
import Variable, Parameter, Constant, Expression, \
    convex, concave, affine, increasing, decreasing, nonmonotone, \
    POSITIVE, NEGATIVE, UNKNOWN, \
    SCALAR, VECTOR, MATRIX
from scoop.ir.coeff import Coeff, Matrix, Eye, Zero
from scoop.ir.dimension import Dimension

# some atoms
@affine
def op_add(self,x,y):
    if isinstance(x,Constant) and isinstance(y,Constant):
        return Constant(x.value + y.value)
        
    # add the arguments to the varlist
    self.varlist[x.name] = x
    self.varlist[y.name] = y
    
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
    
    # create a new variable
    v = self.new_var(shape)
    
    # get dimensions
    dx = self.dimensions.get(x.name, None)
    dy = self.dimensions.get(y.name, None)
    
    if dx and not dy:
        self.dimensions[y.name] = dx
        self.dimensions[v.name] = dx
    elif dy and not dx:
        self.dimensions[x.name] = dy
        self.dimensions[v.name] = dy
    elif not dx and not dy:
        dv = Dimension()
        
        self.dimensions[x.name] = dv
        self.dimensions[y.name] = dv
        self.dimensions[v.name] = dv
    else:
        # these dimension objects exist, but are unknown
        if dx.dim and not dy.dim:
            dy.dim = dx.dim
            self.dimensions[v.name] = dx
        elif dy.dim and not dx.dim:
            dx.dim = dy.dim
            self.dimensions[v.name] = dy
            
            
        # they are equal! (emit a check)
        
    dx = self.dimensions[x.name]
    dy = self.dimensions[y.name]
    dv = self.dimensions[v.name]

        
    
    # # infer dimensions from existing ones (if any)
    # 
    # self.dimensions[v.name] = dv
    # 
    # dx = self.dimensions.get(x.name, dv) #Dimension(Row(v))
    # dy = self.dimensions.get(y.name, dv)
    # 
    # # set the dimensions (but don't overwrite the object)
    # if(dx.dim):
    #     self.dimensions[y.name].dim = dx.dim
    #     self.dimensions[v.name].dim = dx.dim
    # elif(dy.dim):
    #     self.dimensions[x.name].dim = dy.dim
    #     self.dimensions[v.name].dim = dy.dim
    
    # create a row to add to IR        
    self.affine.append(
        [(Eye(dx), x), (Eye(dy), y), (Eye(dv,-1), v), (Zero(dv),)]
    )
    
    return Expression("%s + %s # = %s" % (x.name, y.name, v.name), vex,sign,shape)
    
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

