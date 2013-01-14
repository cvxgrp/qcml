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
module Atoms.Atoms(primitiveAdd, primitiveMinus, primitiveNegate, primitiveMultiply,
  scoop_eq, scoop_leq, scoop_geq, module Atoms.Common) where
  import Expression.Expression
  import Atoms.Common

  -- BELONGS IN ATOMS!
  -- all atoms assume the expression sizes have been "checked"
  primitiveAdd :: Expr -> Expr -> Expression Expr
  primitiveAdd x y = do
    let m = max (rows x) (rows y)
    t <- newVar m

    let v = Affine <&> increasing x <&> increasing y
    let s = case (sign x, sign y) of
              (Positive, Positive) -> Positive
              (Negative, Negative) -> Negative
              otherwise -> Unknown
    -- definition of plus
    addLine $ concat [t, " == ", (name x), " + ", (name y)]
    return $ Expr t m v s

  primitiveMinus :: Expr -> Expr -> Expression Expr
  primitiveMinus x y = do
    let m = max (rows x) (rows y)
    t <- newVar m

    let v = Affine <&> increasing x <&> decreasing y
    let s = case (sign x, sign y) of
              (Positive, Negative) -> Positive
              (Negative, Positive) -> Negative
              otherwise -> Unknown
    -- definition of plus
    addLine $ concat [t, " == ", (name x), " - ", (name y)]
    return $ Expr t m v s

  primitiveNegate :: Expr -> Expression Expr
  primitiveNegate x = do
    t <- newVar (rows x)

    let v = Affine <&> decreasing x
    let s = case (sign x) of
              Positive -> Negative
              Negative -> Positive
              otherwise -> Unknown

    addLine $ concat [t, " == -", (name x)]
    return $ Expr t (rows x) v s

  primitiveMultiply :: Param -> Expr -> Expression Expr
  primitiveMultiply p x = do
    t <- newVar (rows p)

    let v = case (sign p) of
              Positive -> Affine <&> increasing x
              Negative -> Affine <&> decreasing x
              otherwise -> Affine <&> nonmonotone x
    let s = case (sign p, sign x) of
              (Positive, Positive) -> Positive
              (Negative, Negative) -> Positive
              (Positive, Negative) -> Negative
              (Negative, Positive) -> Negative
              otherwise -> Unknown

    addLine $ concat [t, " == ", (name p), "*", (name x)]
    return $ Expr t (rows p) v s

  -- "and" and "find" are atom keywords, so we exclude them

  isConvex :: (Symbolic a) => a -> Bool
  isConvex x
    | vexity x == Convex = True
    | vexity x == Affine = True
    | otherwise = False

  isConcave :: (Symbolic a) => a -> Bool
  isConcave x
    | vexity x == Concave = True
    | vexity x == Affine = True
    | otherwise = False

  scoop_leq :: Symbol -> Symbol -> Expression ()
  scoop_leq x y = do
    let m = max (rows x) (rows y)
    if(isConvex x && isConcave y) then do
      slack <- newVar m
      addLine $ concat [name x, " + ", slack, " == ", name y]
      addLine $ slack ++ " >= 0"
    else
      fail $ "Nonconvex inequality constraint (" ++ show (vexity x) ++ " <= " ++ show (vexity y) ++ ")."

  scoop_geq :: Symbol -> Symbol -> Expression ()
  scoop_geq x y = do
    let m = max (rows x) (rows y)
    if (isConcave y && isConvex x) then do
      slack <- newVar m
      addLine $ concat [name x, " - ", slack, " == ", name y]
      addLine $ slack ++ " >= 0"
    else
      fail $ "Nonconvex inequality constraint (" ++ show (vexity x) ++ " >= " ++ show (vexity y) ++ ")."

  -- a == b
  scoop_eq :: Symbol -> Symbol -> Expression ()
  scoop_eq x y = do
    let (v1, v2) = (vexity x, vexity y)
    case(v1,v2) of
      (Affine, Affine) -> addLine $ concat [name x, " == ", name y]
      otherwise -> fail $ "Nonconvex equality constraint (" ++ show v1 ++ " == " ++ show v2 ++ ")."
