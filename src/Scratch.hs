module Scratch where
  import Rewriter.Atoms
  import Expression.Expression

  data Vector m n = Vector (m,n) deriving (Show)

  f :: Vector m n -> Vector m n -> Vector m n
  f x y = y

  y = Variable "x" (5,1)
  x = Expr "x" Affine Positive (3, 1)
