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
  CVXSense(..),
  CVXProblem(..),
  CVXConstraint(..),
  isConvex, isConcave, isAffine) where
  -- code can be made even shorter if we let Expression have the Functor
  -- and fmap property...
  import Expression.SOCP
  
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
  data CVXSense = Maximize | Minimize deriving (Show, Eq)
  
  data CVXProblem = CVXProblem { 
    sense::CVXSense,
    objective::CVXExpression,
    constraints::[CVXConstraint] 
  }
    
  -- parameter can be promoted to expression
  -- expression cannot be demoted to parameter
  -- XXX: this data structure needs to reflect the paper better
  -- need to have Constant, and remove ParamFunction
  -- functions need to have restrictions
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
    prob (Variable s _ _) = (\_ _ -> Problem (Just (VarId s)) [] [] [])
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
    vexity (Eq lhs rhs)
      | isAffine lhs && isAffine rhs = Convex
      | otherwise = Nonconvex
    vexity (Leq lhs rhs)
      | isConvex lhs && isConcave rhs = Convex
      | otherwise = Nonconvex
    vexity (Geq lhs rhs)
      | isConcave lhs && isConvex rhs = Convex
      | otherwise = Nonconvex
    
  -- problems are "vexable"
  instance Vexable CVXProblem where
    vexity p = 
      let constraintVexList = map vexity (constraints p)
          objectiveVex = reduceVex (vexity $ sense p) (vexity $ objective p)
          problemVex = case(objectiveVex) of
            Convex -> Convex
            Concave -> Convex
            _ -> Nonconvex
      in foldl reduceVex problemVex constraintVexList

  -- problem senses are vexalbe
  instance Vexable CVXSense where
    vexity Minimize = Convex
    vexity Maximize = Concave
        
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
  
  -- testing if isConvex, isConcave, isAffine
  isConvex :: Vexable a => a -> Bool
  isConvex x = (v == Convex) || (v == Affine)
    where v = vexity x
  
  isConcave :: Vexable a => a -> Bool
  isConcave x = (v == Concave) || (v == Affine)
    where v = vexity x
  
  isAffine :: Vexable a => a -> Bool
  isAffine x = (v == Affine)
    where v = vexity x
