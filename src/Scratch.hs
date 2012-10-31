module Scratch where
  import Rewriter.Atoms
  import Expression.Expression
  
  y = Variable "x" (5,1)
  x = Expr "x" Affine Positive (3, 1)