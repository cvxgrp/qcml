module Expression.DCP (Monotonicity(..), 
  Vexity(..),
  Sign(..),
  Expression(..),
  CVXSymbol(..),
  CVXExpression,
  Vexable(..),
  Signable(..),
  Prob(..),
  SignFunc,
  MonoFunc,
  ProblemFunc,
  CVXProblem(..),
  CVXConstraint(..)) where
  -- code can be made even shorter if we let Expression have the Functor
  -- and fmap property...
  import Problem.SOCP
  
  -- data types for signed monotonicity DCP rules
  data Monotonicity = Increasing | Decreasing | Nonmonotone 
    deriving (Show, Eq)
  data Vexity = Convex | Concave | Affine | Nonconvex 
    deriving (Show, Eq)
  data Sign = Positive | Negative | Unknown
    deriving (Show, Eq)

  class Vexable a where
    vexity :: a -> Vexity

  class Signable a where
    sign :: a -> Sign
  
  class SFunc a where
    sfunc :: a -> SignFunc
  
  class MFunc a where
    mfunc :: a -> MonoFunc
  
  class Prob a where
    prob :: a -> ProblemFunc

  -- collect the properties of a node together
  class (Vexable a, SFunc a, MFunc a, Prob a) => Node a where
  -- collect the properties of an expression together
  class (Vexable a, Signable a) => Expr a where
    
  -- expression data type
  data Expression a = Empty 
    | Leaf a 
    | BinaryNode a (Expression a) (Expression a)
    | UnaryNode a (Expression a)
  
  -- typedefs to reduce "typing" (characters) of code
  type CVXExpression = Expression CVXSymbol
  type SignFunc = [Sign]->Sign
  type MonoFunc = [Sign]->[Monotonicity]
  type ProblemFunc = VarId -> [VarId] -> Problem
  
  -- constraint data type
  data CVXConstraint = Eq {lhs::CVXExpression, rhs::CVXExpression}
    | Leq {lhs::CVXExpression, rhs::CVXExpression}
    | Geq {lhs::CVXExpression, rhs::CVXExpression}
    
  -- problem data type (all problems are convex!)
  data CVXProblem = CVXProblem { objective::CVXExpression,
    constraints::[CVXConstraint] }
    
  -- parameter can be promoted to expression
  -- expression cannot be demoted to parameter
  data CVXSymbol
    = Parameter { 
      name::String,
      symbolVexity::Vexity, 
      symbolSign::SignFunc,
      symbolRewrite::ProblemFunc
    }
    | Variable { 
      name::String,
      symbolVexity::Vexity, 
      symbolSign::SignFunc
    }
    | Function { 
      name::String,
      nargs::Int,
      symbolVexity::Vexity, 
      symbolSign::SignFunc, 
      monotonicity::MonoFunc,
      symbolRewrite::ProblemFunc
    }
    | ParamFunction {
      name::String, 
      nargs::Int,
      symbolVexity::Vexity, 
      symbolSign::SignFunc, 
      monotonicity::MonoFunc,
      symbolRewrite::ProblemFunc
    }
  
  -- symbols are vexable (can ask for its vexity)
  instance Vexable CVXSymbol where
    vexity x = symbolVexity x
  
  -- symbols are "SFunc"-able (you can ask for a sign function)
  instance SFunc CVXSymbol where
    sfunc x = symbolSign x
  
  -- symbols are "MFunc"-able (you can ask for monotonicity function)
  instance MFunc CVXSymbol where
    mfunc (Parameter _ _ _ _) = (\_ -> [])
    mfunc (Variable _ _ _) = (\_ -> [])
    mfunc x = monotonicity x
  
  -- symbols are "PFunc"-able (you can ask for a problem rewriter)
  instance Prob CVXSymbol where
    prob (Variable s _ _) = (\_ _ -> Problem (VarId s) [] [] [])
    prob x = symbolRewrite x
  
  -- symbols are Nodes (a node is just Vexable, SFunc, and MFunc)
  instance Node CVXSymbol where
  
  -- expressions are vexable (can ask for its vexity)
  instance (Node a) => Vexable (Expression a) where
    vexity (Leaf x) = vexity x
    vexity (BinaryNode x left right) = 
        infer x [left,right]
    vexity (UnaryNode x rest) =
        infer x [rest]
    vexity _ = Nonconvex

  -- expressions are signable (can ask for its sign)
  instance (Node a) => Signable (Expression a) where
    sign (Leaf x) = sfunc x []
    sign (BinaryNode x left right) =
      sfunc x [sign left, sign right]
    sign (UnaryNode x rest) = 
      sfunc x [sign rest]
    sign _ = Unknown
  
  -- expressions are made up of nodes and are of class Expr
  -- the Expr class just says it's signable and vexable
  instance (Node a) => Expr (Expression a) where
    
  -- constraints are "vexable"
  instance Vexable CVXConstraint where
    vexity (Eq lhs rhs) = case (vexity lhs, vexity rhs) of
      (Affine, Affine)-> Convex
      _ -> Nonconvex
    vexity (Leq lhs rhs) = case (vexity lhs, vexity rhs) of
      (Convex, Affine) -> Convex
      (Affine, Affine) -> Convex
      _ -> Nonconvex
    vexity (Geq lhs rhs) = case (vexity lhs, vexity rhs) of
      (Concave, Affine) -> Convex
      (Affine, Affine) -> Convex
      _ -> Nonconvex
    
  -- problems are "vexable"
  instance Vexable CVXProblem where
    vexity p = foldl reduceVex (vexity $ objective p) (map vexity (constraints p))
        
  -- symbols can be "shown"
  instance Show CVXSymbol where
    show x = name x

  -- expressions can be "shown"
  instance (Show a) => Show (Expression a) where
    show (Leaf x) = show x
    show (BinaryNode x y z) 
      = (show x) ++ "(" ++ show y ++ ", " ++ show z ++")"
    show (UnaryNode x y) 
      = (show x) ++ "(" ++ show y ++ ")"
    show _ = "Empty Expression"

  -- constraints can be "shown"
  instance Show CVXConstraint where
    show (Eq lhs rhs) = (show lhs) ++ " == " ++ (show rhs) 
    show (Leq lhs rhs) = (show lhs) ++ " <= " ++ (show rhs) 
    show (Geq lhs rhs) = (show lhs) ++ " >= " ++ (show rhs) 
    
  -- problems can be "shown"
  instance Show CVXProblem where
    show p = unwords $ ["minimize " ++ (show $ (objective p)),
      "subject to"] ++ (map show (constraints p))


  -- helper functions for DCP rules
  infer :: (Node a, Expr b) => a -> [b] -> Vexity
  infer x y = 
    let m = mfunc x (map sign y)
        v = map vexity y
    in foldl reduceVex (vexity x) (zipWith inferVexity v m)

  -- composition rule
  inferVexity :: Vexity -> Monotonicity -> Vexity
  inferVexity Convex Increasing = Convex
  inferVexity Convex Decreasing = Concave
  inferVexity Concave Decreasing = Convex
  inferVexity Concave Increasing = Concave
  inferVexity Affine _ = Affine
  inferVexity _ _ = Nonconvex
  
  -- given a list of vexities, gives the "max"
  -- i.e., Nonconvex > Affine > Convex = Concave
  -- er, just produces the "common" vexity
  -- [Affine, Affine, Affine] -> Affine
  -- [Affine, Convex, Convex] -> Convex
  -- etc.
  -- useful for inferring vexity of function (gives vexity in each arg)
  reduceVex :: Vexity -> Vexity -> Vexity
  reduceVex Nonconvex _ = Nonconvex
  reduceVex _ Nonconvex = Nonconvex
  reduceVex Affine s = s
  reduceVex s Affine = s
  reduceVex Convex Convex = Convex
  reduceVex Concave Concave = Concave
  reduceVex _ _ = Nonconvex
