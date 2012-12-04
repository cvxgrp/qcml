module Scratch where
  import Atoms.Atoms
  import Expression.Expression

  import CodeGenerator.CVX
  import CodeGenerator.CVXSOCP
  import CodeGenerator.ECOS

  -- for monadic atoms (will allow us to produce very clean Haskell code)
  -- won't be easy to translate into C (must have good documentation / explanation)
  import Control.Monad.State



  x = variable "x" (5,1)
  a = parameter "A" Positive (3,5)
  b = parameter "b" Unknown (3,1)
  d = parameter "s" Positive (1,1)

  -- could really monad it up to make it look imperative *and* almost embedded
  v = scoop_sum (scoop_square (scoop_minus (scoop_mult a x "1") b "3") "4") "5"

  -- let's make parameters, variables, expressions part of a typeclass


  incr :: State Int Int
  incr = do
    s <- get
    put (s+1)
    get

  mscoop_sum :: State Int Expr -> State Int Expr
  mscoop_sum x = do
    s <- incr
    a <- x
    return (scoop_sum a (show s))
  --mscoop_sum (State a x) = State (scoop_sum a (show x)) (x+1)
