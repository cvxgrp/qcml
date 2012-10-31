module Scratch where
  import Rewriter.Atoms
  import Expression.Expression

  x = variable "x" (5,1)
  a = parameter "A" Positive (3,5)
  b = parameter "b" Unknown (3,1)
