from scoop_expression import Variable, Parameter, Constant, Expression, \
    convex, concave, affine, increasing, decreasing, nonmonotone

class Evaluator(object):
    # counter for new variables introduced (private?)
    varcount = 0
    symtable = {} # i don't actually need a symtable...
    
    def __init__(self,table):
        self.symtable = table
    
    def hello(self):
        pass
    
    def new_var(self,shape):
        name = 't' + str(self.varcount)
        self.varcount += 1
        return Variable(name, shape)

    # some atoms
    @affine
    def op_add(self,x,y):
        if isinstance(x,Constant) and isinstance(y,Constant):
            return Constant(x.value + y.value)
            
        v = increasing(x) | increasing(y)
    
        # what does it do?
        # every Operand has a .rewrite associated with it
        # a row is
        # [(Parameter, Variable), (Parameter, Variable), .., (Parameter,)]
        # a cone constraint is
        # no, for the cones, we will form
        # (SCALAR Variable, Variable, Variable) # in (t,x,y,z) form
        # (VECTOR Variable, Variable) # this is multiarg form |x|<=y
        # (Variable, ZERO) # this is x >= 0
        # we'll pre-process this list before codegen, which will get us G, h
        # and will also form "struct" directly
        
        # printing is just a matter of printing the symbol table
        # then each *row* of Ax = b
        # then the list of cones
        
        # codegen is just a matter of gen-ing each *row* of Ax = b
        # then converting IR form to G,h, then gen-ing each *row* of 
        # Gx + s = h
        
        # that will produce a bunch of constraints and give the top-level name
    
        return Expression(v)
    
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
    
    # 1) as i parse, any "variables" or "parameters" will get created in the IR symbol table (separate lists)
    
    # 2) when Evaluator is run, it first checks to see if any of the special
    # cases are satisfied across the *entire* line
    # these are:
    # [VARIABLE, ZERO, GEQ] -- "variable len" linear cone (SOC 1)
    # [VARIABLE, ABS, VARIABLE, LEQ] -- "variable len" # of (SOC 2)
    # [VARIABLE, NORM, SCALAR VARIABLE, LEQ] -- "1" # of (SOC VARLEN + 1)
    # [VARIABLE, ..., NORM, VARIABLE, LEQ] -- "variable len" # of (SOC # args + 1)
    # so the usual EYE is
    # EYE(start=0,stride=1) generates -> [1 0; 0 1]
    # EYE(start=0,stride=2) generates -> [1 0; 0 0; 0 1; 0 0], but
    # EYE(start=1,stride=2) generates -> [0 0; 1 0; 0 0; 0 1]
    
    # otherwise, it will just consume using an RPN algorithm