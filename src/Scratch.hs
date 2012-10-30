module Scratch( f) where
    
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
  
  -- defined as problems
  data Problem = Problem deriving (Show) 
  
  -- a "thing" can be tagged as an expression with curvature and sign or 
  -- as an atom with curvature, monotonicity, and sign
  data Tagged a 
    = Expr Sign Curvature a
    | Atom Sign Curvature Monotonicity a
    deriving (Show)
    
  g :: Tagged Problem
  g = Atom Positive Convex Increasing Problem
  
  -- let's put this in a typeclass
  class DCP a where
    
  
  -- these things don't even *check* DCP
  -- atoms just describe how to "compose" problems
  quad_over_lin :: Problem -> Problem -> Problem
  quad_over_lin x y = x
  
  -- checks DCP rule
  -- apply :: Atom a -> Expr -> Expr
  --   apply (Atom curvature _ s _) (Expr Affine _) = Expr curvature s
  --   apply (Atom Convex Increasing s _) (Expr Convex _) = Expr Convex s
  --   apply (Atom Convex Decreasing s _) (Expr Concave _) = Expr Concave s
  --   apply (Atom Concave Decreasing s _) (Expr Convex _) = Expr Convex s
  --   apply (Atom Concave Increasing s _) (Expr Concave _) = Expr Concave s
  --   apply _ _ = Expr Nonconvex Unknown
  --   
  --   
  --   alist = 
  --     [
  --       ("square", Atom Convex Nonmonotone Positive square),
  --       ("quad_over_lin", Atom Convex Nonmonotone Positive quad_over_lin)
  --     ]
  --     
  --   -- square is an ATOM!!
  --   square :: Expr -> Expr
  --   square x = x
  --   
  --   quad_over_lin :: Expr -> Expr -> Expr
  --   quad_over_lin x y = y
  
  -- g :: Expr -> Int
  -- g x = val x
  
  f :: (Int,Int) -> Int
  f a = case (a) of
    (x,y) | x == y -> x 
    otherwise -> 1