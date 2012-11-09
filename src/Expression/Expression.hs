module Expression.Expression (
  Curvature(..), 
  Monotonicity(..), 
  Sign(..), 
  Expr(..), 
  Symbol(..),
  Rewriteable(..),
  applyDCP,
  expression,
  variable,
  parameter,
  none,
  module Expression.SOCP
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
    = Expr Var Curvature Sign ConicSet
    | Variable Var
    | Parameter String Sign (Int, Int)
    | Constant Double
    | None String

  instance Show Expr where
    show (Expr v c s _) = vname v ++ " = " ++ show s ++ " " ++ show c ++ " Expr"
    show (Variable v) = "Variable " ++ vname v ++ (show $ vshape v)
    show (Parameter s sgn size) = (show sgn) ++ " Parameter " ++ s ++ (show size)
    show (Constant x) = "Constant " ++ (show x)
    show (None s) = s
  
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
    name (Variable v) = vname v
    name (Parameter s _ _) = s
    name (Constant x) = display x
    
    vexity (None _) = Nonconvex
    vexity (Variable _) = Affine
    vexity (Expr _ c _ _) = c
    vexity (Parameter _ _ _) = Affine
    vexity (Constant _) = Affine
  
    sign (None _) = Unknown
    sign (Variable _) = Unknown
    sign (Expr _ _ s _) = s
    sign (Parameter _ s _) = s
    sign (Constant x)
      | x >= 0 = Positive
      | x < 0 = Negative
      | otherwise = Unknown
    
    rows (None _) = 0
    rows (Expr v _ _ _) = vrows v
    rows (Variable v) = vrows v
    rows (Parameter _ _ (m,_)) = m
    rows (Constant _) = 1
    
    cols (None _) = 0
    cols (Expr v _ _ _) = vcols v
    cols (Variable v) = vcols v
    cols (Parameter _ _ (_,n)) = n
    cols (Constant _) = 1

  -- type class to enable code generation
  class Rewriteable a where
    socp :: a -> SOCP
    var :: a -> Var
    cones :: a -> ConicSet

  instance Rewriteable Expr where
    socp (Expr v c _ p)
      | c == Convex = SOCP Minimize v p
      | c == Concave = SOCP Maximize v p
      | otherwise = SOCP Find v p
    socp (Variable v) = SOCP Find v (ConicSet [] [] [])
    -- parameter cast occurs here
    socp (Parameter v _ (m,n)) = SOCP Find newVar (ConicSet matA vecB [])
      where newVar = Var ("p"++v) (m,n)
            matA = [ Row [(Eye m 1, newVar)] ]
            vecB = [Vector m v]
    socp (Constant x) = SOCP Find newVar (ConicSet matA vecB [])
      where newVar = Var ("c"++(display x)) (1,1)
            matA = [ Row [(Ones 1 1, newVar)] ]
            vecB = [Ones 1 x]
    socp (None _) = SOCP Find (Var "0" (1,1)) (ConicSet [] [] [])

    var (Expr v _ _ _) = v
    var (Variable v) = v
    -- creates new var for parameter
    var (Parameter v _ sz) = Var ("p"++v) sz
    var (Constant x) = Var ("c"++(display x)) (1,1)
    var (None _) = Var "0" (1,1)

    cones (Expr _ _ _ k) = k
    cones (Variable _) = ConicSet [] [] []
    cones (Parameter v _ (m,n))= ConicSet matA vecB []
      where matA = [ Row [(Eye m 1, Var ("p"++v) (m,n))] ]
            vecB = [Vector m v]
    cones (Constant x) = ConicSet matA vecB []
      where matA = [ Row [(Ones 1 1, Var ("c"++(display x)) (1,1))] ]
            vecB = [Ones 1 x]
    cones (None _) = ConicSet [] [] []

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
  expression :: Var -> Curvature -> Sign -> ConicSet -> Expr
  expression v c s k = Expr v c s k
  
  variable :: String -> (Int, Int) -> Expr
  variable name (m,n) = Variable newVar
    where newVar = Var name (m,n)
  
  none :: String -> Expr
  none s = None s

  display :: Double -> String
  display = (map (\x -> if (x=='.') then 'd' else x)).show
  
  parameter = Parameter
