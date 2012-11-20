module Expression.Expression (
  Curvature(..), 
  Monotonicity(..), 
  Sign(..),
  ShapeMod(..),
  Expr(..), 
  DCP(..),
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

  data ShapeMod   -- shape modifier for parameters
    = Transposed
    | Diagonal
    | NoMod
    deriving (Show, Eq)

  data Expr
    = Expr Var Curvature Sign ConicSet
    | Variable Var
    | Parameter Param Sign ShapeMod
    | Constant Double
    | None String

  instance Show Expr where
    show (Expr v c s _) = name v ++ " = " ++ show s ++ " " ++ show c ++ " Expr"
    show (Variable v) = "Variable " ++ name v ++ (show $ dimensions v)
    show (Parameter s sgn _) = (show sgn) ++ (show s)
    show (Constant x) = "Constant " ++ (show x)
    show (None s) = s
  
  instance Symbol Expr where
    name (None s) = s
    name (Expr v _ _ _) = name v
    name (Variable v) = name v
    name (Parameter s _ _) = name s
    name (Constant x) = display x

    rows (None _) = 0
    rows (Expr v _ _ _) = rows v
    rows (Variable v) = rows v
    rows (Parameter p _ Transposed) = cols p
    rows (Parameter p _ Diagonal) = rows p
    rows (Parameter p _ NoMod) = rows p
    rows (Constant _) = 1
    
    cols (None _) = 0
    cols (Expr v _ _ _) = cols v
    cols (Variable v) = cols v
    cols (Parameter p _ Transposed) = rows p
    cols (Parameter p _ Diagonal) = rows p
    cols (Parameter p _ NoMod) = cols p
    cols (Constant _) = 1

    dimensions x = (rows x, cols x)

  -- type classes to enable vexity inference
  class DCP a where
    vexity :: a -> Curvature
    sign :: a -> Sign

  instance DCP Expr where
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
    -- parameter and constant casting occurs here
    socp (Parameter p _ s) = parameterSOCP p s
    socp (Constant x) = constantSOCP x
    socp (None _) = SOCP Find (Var "0" (1,1)) (ConicSet [] [] [])

    var (Expr v _ _ _) = v
    var (Variable v) = v
    var (Constant x) = obj (constantSOCP x)
    var (None _) = Var "0" (1,1)
    var (Parameter p _ s) = obj (parameterSOCP p s)

    cones (Expr _ _ _ k) = k
    cones (Variable _) = ConicSet [] [] []
    cones (Parameter p _ s) = constraints (parameterSOCP p s)
    cones (Constant x) = constraints (constantSOCP x)
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

  parameter = Parameter

  display :: Double -> String
  display = (map (\x -> if (x=='.') then 'd' else x)).show
  
  -- helper function to construct SOCP for parameters and constants
  parameterSOCP :: Param -> ShapeMod -> SOCP  -- ignores the shape modifier (just introduces param via eq constraints)
  parameterSOCP (Param s (m,1)) NoMod = SOCP Find newVar (ConicSet matA vecB [])
    where newVar = Var ("p"++s) (m,1)
          matA = [Row [(Eye m 1, newVar)]]
          vecB = [Vector m (Param s (m,1))]
  parameterSOCP (Param s (1,m)) Transposed = SOCP Find newVar (ConicSet matA vecB [])
    where newVar = Var ("p"++s) (m,1)
          matA = [Row [(Eye m 1, newVar)]]
          vecB = [VectorT m (Param s (1,m))]
  parameterSOCP _ _ = SOCP Find (Var "0" (1,1)) (ConicSet [] [] [])

  constantSOCP :: Double -> SOCP
  constantSOCP x = SOCP Find newVar (ConicSet matA vecB [])
    where newVar = Var ("c"++(display x)) (1,1)
          matA = [ Row [(Ones 1 1, newVar)] ]
          vecB = [Ones 1 x]
