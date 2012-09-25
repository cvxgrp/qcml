module Rewriter.ECOS (isDCP, testme, Vexity(..), Monotonicity(..), Sign(..), Node(..)) where
  import qualified Data.Map as M
  
  -- data types for annotating the expression tree
  data Vexity = Convex
    | Concave
    | Affine
    | Nonconvex
    deriving (Show)

  data Monotonicity = Increasing
    | Decreasing
    | Nonmonotone
    deriving (Show)

  data Sign = Positive
    | Negative
    | Unknown
    deriving (Show)
  
  data Node = Var String Sign
    | Const Double
    | BinaryOp String Monotonicity Monotonicity
    | UnaryOp String Monotonicity
    deriving (Show)

  type Expression = [Node]
    
  ecos_quad_over_lin :: Double -> Double -> Double
  ecos_quad_over_lin x y = (x*x)/y
  
  ecos_square :: Double -> Double -> Double
  ecos_square x y = (\x -> ecos_quad_over_lin x 1) x
  
  ecos_inv_pos :: Double -> Double -> Double
  ecos_inv_pos x y = ecos_quad_over_lin 1 x
  
  ecos_abs :: Double -> Double -> Double
  ecos_abs x y = abs x
  
  testme :: String -> Double
  testme s = case (M.lookup s $ atoms) of
    Just x -> x 3 4
    Nothing -> 0
  
  -- define the atoms
  atoms = M.fromList [
      ("quad_over_lin", ecos_quad_over_lin),
      ("square", ecos_square),
      ("inv_pos", ecos_inv_pos),
      ("abs", ecos_abs)
      ]
      
  -- above is just a test
  -- ecos_plus :: Expression -> Expression -> Expression
  -- ecos_plus a b = a ++ b ++ [BinaryOp "+"]
  -- 
  -- ecos_sub :: Expression -> Expression -> Expression
  -- ecos_sub a b = a ++ b ++ [BinaryOp "-"]
  -- 
  -- ecos_mul :: Expression -> Expression -> Expression
  -- ecos_mul a b = a ++ b ++ [BinaryOp "*"]
  -- 
  -- ecos_quad_over_lin' :: Expression -> Expression -> Expression
  -- ecos_quad_over_lin' a b = a ++ b ++ [BinaryOp "quad_over_lin"]
    
  -- isDCP :: Expression -> Bool
  -- isDCP expr = isDCP' [] expr
  -- 
  -- isDCP' :: [(Vexity, Sign)] -> Expression -> Bool
  -- isDCP' _ [] = True
  -- isDCP' prev (Var n s:expr) = isDCP' ((Affine, s):prev) expr
  -- isDCP' prev (Const d:expr)
  --   | d >= 0 = isDCP' ((Affine, Positive):prev) expr
  --   | otherwise = isDCP' ((Affine, Negative):prev) expr
  -- isDCP' prev (BinaryOp s Increasing Increasing sign:expr) = case prev of
  --     ((Convex,_):(Convex,_):rest) -> isDCP' ((Convex, sign):rest) expr
  --     ((Affine,_):(Affine,_):rest) -> isDCP' ((Convex, sign):rest) expr
  --     ((Concave,_):(Concave,_):rest) -> isDCP' ((Convex, sign):rest) expr
  --     otherwise -> False
  -- isDCP' prev (UnaryOp s Increasing sign:expr) = case prev of
  --     ((Convex,_):rest) -> isDCP' ((Convex, sign):rest) expr
  --     ((Affine,_):rest) -> isDCP' ((Affine, sign):rest) expr
  --     ((Concave,_):rest) -> isDCP' ((Concave, sign):rest) expr
  --     otherwise -> False
  

  
  -- binaryOps = M.fromList [
  --   ("plus", ecos_plus),
  --   ("sub", ecos_sub),
  --   ("mul", ecos_mul),
  --   ("quad_over_lin", ecos_quad_over_lin')
  --   ]
  
  