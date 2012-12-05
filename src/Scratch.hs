{--

Copyright (c) 2012-2013, Eric Chu (eytchu@gmail.com)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met: 

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. 
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are
those of the authors and should not be interpreted as representing official
policies, either expressed or implied, of the FreeBSD Project.

--}

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

  mscoop_sum :: [Expr] -> State Int Expr
  mscoop_sum x = do
    s <- incr
    return (scoop_sum x (show s))
  --mscoop_sum (State a x) = State (scoop_sum a (show x)) (x+1)
