module Expression.Expression (
  Curvature(..), 
  Monotonicity(..), 
  Sign(..), 
  Expr, 
  Parameter, 
  Symbol(..),
  Rewriteable(..),
  applyDCP,
  expression,
  variable,
  parameter,
  none
) where

  import Expression.SOCP
  
  -- data types
  data Curvature
    = Convex
    | Concave
    | Affine
    | Nonconvex
    deriving (Show, Eq)

  data Monotonicity
    = Increasing
    | Decreasing
    | Nonmonotone
    deriving (Show, Eq)

  data Sign
    = Positive
    | Negative
    | Unknown
    deriving (Show, Eq)

  data Expr
    = Expr Var Curvature Sign Cones
    | Variable Var Cones
    | None String

  instance Show Expr where
    show (Expr v c s _) = vname v ++ " = " ++ show s ++ " " ++ show c ++ " Expr"
    show (Variable v _) = "Variable " ++ vname v ++ (show $ vshape v)
    show (None s) = s

  data Parameter = Parameter String Sign (Int, Int) deriving (Show)
  
  -- type classes to enable vexity inference
  class Symbol a where
    name :: a -> String
    vexity :: a -> Curvature
    sign :: a -> Sign
    rows :: a -> Int
    cols :: a -> Int

  instance Symbol Expr where
    name (None s) = s
    name (Expr v _ _ _) = vname v
    name (Variable v _) = vname v
    
    vexity (None _) = Nonconvex
    vexity (Variable _ _) = Affine
    vexity (Expr _ c _ _) = c
    
    sign (None _) = Unknown
    sign (Variable _ _) = Unknown
    sign (Expr _ _ s _) = s
    
    rows (None _) = 0
    rows (Expr v _ _ _) = vrows v
    rows (Variable v _) = vrows v
    
    cols (None _) = 0
    cols (Expr v _ _ _) = vcols v
    cols (Variable v _) = vcols v

  instance Symbol Parameter where
    name (Parameter s _ _) = s
    vexity (Parameter _ _ _) = Affine
    sign (Parameter _ s _) = s
    rows (Parameter _ _ (m,_)) = m
    cols (Parameter _ _ (_,n)) = n

  -- type class to enable code generation
  class Rewriteable a where
    socp :: a -> SOCP
    var :: a -> Var
    cones :: a -> Cones

  instance Rewriteable Expr where
    socp (Expr v c _ p)
      | c == Convex = SOCP Minimize v p
      | c == Concave = SOCP Maximize v p
      | otherwise = SOCP Find v p
    socp (Variable v p) = SOCP Find v p
    socp (None _) = SOCP Find (Var "0" (1,1)) (Cones [] [] [])

    var (Expr v _ _ _) = v
    var (Variable v _) = v
    var (None _) = Var "0" (1,1)

    cones (Expr _ _ _ k) = k
    cones (Variable _ k) = k
    cones (None _) = Cones [] [] []

  -- DCP rules
  applyDCP :: Curvature -> Monotonicity -> Curvature -> Curvature
  applyDCP Convex Increasing Convex = Convex
  applyDCP Convex Decreasing Concave = Convex
  applyDCP Concave Decreasing Convex = Concave
  applyDCP Concave Increasing Concave = Concave
  applyDCP c _ Affine = c
  applyDCP Affine Increasing c = c
  applyDCP Affine Decreasing c = flipVexity c
  applyDCP _ _ _ = Nonconvex

  flipVexity :: Curvature -> Curvature
  flipVexity Concave = Convex
  flipVexity Convex = Concave
  flipVexity Affine = Affine
  flipVexity Nonconvex = Nonconvex
  
  -- constructors for expr, variables, parameter, and none
  -- TODO: SOCP problem keeps track of top-level variable and also has arbitrary sense
  -- TODO: need to have user-specified problem adjust the socp sense
  expression :: Var -> Curvature -> Sign -> Cones -> Expr
  expression v c s k = Expr v c s k
  
  variable :: String -> (Int, Int) -> Expr
  variable name (m,n) = Variable newVar (Cones [] [] [])
    where newVar = Var name (m,n)
  
  none :: String -> Expr
  none s = None s
  
  parameter :: String -> Sign -> (Int, Int) -> Parameter
  parameter name s (m,n) = Parameter name s (m,n)

  --varName = vname.obj
  --varShape = vshape.obj
  --varCols = vcols.obj
  --varRows = vrows.obj


