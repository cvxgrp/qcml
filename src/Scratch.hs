module Scratch where
  import Atoms.Atoms
  import Expression.Expression

  import CodeGenerator.CVX
  import CodeGenerator.CVXSOCP
  import CodeGenerator.ECOS

  x = variable "x" (5,1)
  a = parameter "A" Positive (3,5)
  b = parameter "b" Unknown (3,1)
  d = parameter "s" Positive (1,1)

  -- could really monad it up to make it look imperative *and* almost embedded
  v = ecos_sum (ecos_square (ecos_minus (ecos_mult a x "1") b "3") "4") "5"
