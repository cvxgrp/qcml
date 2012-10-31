module Scratch where
  import Rewriter.Atoms
  import Expression.Expression

  x = Variable "x" (5,1)
  a = Parameter "A" Positive (3,5)
  b = Parameter "b" Unknown (3,1)
