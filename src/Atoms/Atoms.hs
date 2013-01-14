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
  -- all atoms assume the Rewriter sizes have been "checked"
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


  primitiveAdd :: Expr -> Expr -> Rewriter Expr
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

  primitiveMinus :: Expr -> Expr -> Rewriter Expr
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

  primitiveNegate :: Expr -> Rewriter Expr
  primitiveNegate x = do
    t <- newVar (rows x)

    let v = Affine <&> decreasing x
    let s = case (sign x) of
              Positive -> Negative
              Negative -> Positive
              otherwise -> Unknown

    addLine $ concat [t, " == -", (name x)]
    return $ Expr t (rows x) v s

  primitiveMultiply :: Param -> Expr -> Rewriter Expr
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

  -- operations for creating statements
  scoop_leq :: Symbol -> Symbol -> Rewriter ()
  scoop_leq x y = do
    let m = max (rows x) (rows y)
    if(isConvex x && isConcave y) then do
      slack <- newVar m
      addLine $ concat [name x, " + ", slack, " == ", name y]
      addLine $ slack ++ " >= 0"
    else
      fail $ "Nonconvex inequality constraint (" ++ show (vexity x) ++ " <= " ++ show (vexity y) ++ ")."

  scoop_geq :: Symbol -> Symbol -> Rewriter ()
  scoop_geq x y = do
    let m = max (rows x) (rows y)
    if (isConcave y && isConvex x) then do
      slack <- newVar m
      addLine $ concat [name x, " - ", slack, " == ", name y]
      addLine $ slack ++ " >= 0"
    else
      fail $ "Nonconvex inequality constraint (" ++ show (vexity x) ++ " >= " ++ show (vexity y) ++ ")."

  -- a == b
  scoop_eq :: Symbol -> Symbol -> Rewriter ()
  scoop_eq x y = do
    let (v1, v2) = (vexity x, vexity y)
    case(v1,v2) of
      (Affine, Affine) -> addLine $ concat [name x, " == ", name y]
      otherwise -> fail $ "Nonconvex equality constraint (" ++ show v1 ++ " == " ++ show v2 ++ ")."


  -- TODO: delay dimension checking until code generation (when dimensions become concrete)
  -- alternative 1 -- do dimension checking with parser
  --               -- requires recognizing that "m+n+1" == "n+m+1" (sort the string... problem with "m+n+3" and "2+mn+1")
  --               -- requires knowing that "m+m+m+m" == "4*m"
  --               -- requires constant folding "m+2" == "m+1+1"
  -- requires writing more code for the parser to ensure that dimensions match (in abstract sense)
  --
  -- alternative 2 -- do dimension checking with code generator
  -- much easier, but lets people write things like "A*x", where A is param(m,n) and x is variable(s)
  -- and then when they run codegen, they don't pick the right values of m,n, and s.
  -- counter: easy enough to point out that supplied values of n and s don't match. 
  -- counter: also like CVX, could declare m = 3, n =2, A = randn(m,n); s = 2, variable x(s), A*x
  --
  -- can create dimension alias?
  --    dimension m = n
  -- code generator now checks that when the user defines the dimensions m and n, they *must* be the same
  -- this leaves shte Rewriter monad "fail" free (it should never fail)

  -- consider this expression max([x;y]), where x is variable(m), y is variable(n)
  -- this creates a new variable, but of what size? (the dimension can't be fixed).
  -- create a new dimension? let codegen infer?

  -- also if user declares variable t0, then that will conflict with our internal naming.
  -- stick a flag on top of file to signal IR?
  --
  -- _variable's are rewriter-created
  -- their dimensions are automatically inferred (during codegen)

{--
  -- operations that create Expr
  -- t = [x;y;z; ...]
  --    concatenation will be code generator operation
  scoop_concat :: [Expr] -> Rewriter Expr
  scoop_concat xs = do
    let v = foldr (\y vex -> vex <&> increasing y) Affine xs
    let s | all (==Positive) (map sign xs) = Positive
          | all (==Negative) (map sign xs) = Negative
          | otherwise = Unknown

  x s = expression newVar curvature sgn prog
    where
      -- starts with Affine vexity
      -- each argument is Increasing
      -- fold across entire array using previously determined partial vexity
      -- result is "global" vexity (of entire vector)
      curvature = foldr (\y vex -> applyDCP vex Increasing (vexity y)) Affine x
      sgn
        | all (==Positive) (map sign x) = Positive
        | all (==Negative) (map sign x) = Negative
        | otherwise = Unknown
      prog = foldr (<++>) (ConicSet matA vecB [])  (map cones x)
      sizes = map rows x
      m = foldr (+) 0 sizes -- cumulative sum of all rows
      newVar = Var ("t"++s) (m,1)
      coeffs = zip (map (flip Eye 1) sizes) (map var x)
      matA = [Row ((Eye m (-1), newVar):coeffs) ] -- the *first* of this list *must* be the variable to write *out*, the result of concatenation (otherwise the code will break)
      vecB = [Ones m 0]
  -- operators on parameters

  -- transpose a = a' (new parameter named " a' ")
  -- this works perfectly in Matlab, but care must be taken at
  -- codegen to parse a "tick" as a transposed parameter
  scoop_transpose :: Expr -> Expr
  scoop_transpose (None s) = None s
  scoop_transpose (Parameter p psgn NoMod) = Parameter p psgn Transposed
  scoop_transpose (Parameter p psgn Transposed) = Parameter p psgn NoMod
  scoop_transpose (Parameter p psgn Diagonal) = None $ "transpose: can only transpose vectors"
  scoop_transpose x = None $ "transpose: cannot transpose " ++ (name x) ++ "; can only transpose parameters"

  -- diag a = diag(a) (new diagonal matrix parameter)
  scoop_diag :: Expr -> Expr
  scoop_diag (None s) = None s
  scoop_diag (Parameter (Param s (m,1)) psgn NoMod) = Parameter (Param s (m,1)) psgn Diagonal
  scoop_diag (Parameter (Param s (1,m)) psgn Transposed) = Parameter (Param s (1,m)) psgn Diagonal
  scoop_diag (Parameter p psgn Diagonal) = Parameter p psgn Diagonal -- does nothing
  scoop_diag x = None $ "diag: cannot diagonalize " ++ (name x) ++ "; can only diagonalize vector parameters"



-- square x = x^2
  scoop_square :: Expr -> String -> Expr
  scoop_square (None s) _ = None s
  scoop_square x s = expression newVar curvature Positive prog
    where
      curvature = applyDCP Convex monotonicity (vexity x)
      monotonicity = case (sign x) of
        Positive -> Increasing
        Negative -> Decreasing
        otherwise -> Nonmonotone
      prog = (ConicSet matA vecB kones) <++> (cones x)
      (m,n) = (rows x, cols x)
      newVar = Var ("t"++s) (m, n)
      z0 = Var (name newVar ++ "z0") (m, n)
      z1 = Var (name newVar ++ "z1") (m, n)
      matA = [ Row [(Eye m 0.5, newVar), (Eye m (-1), z0)],
               Row [(Eye m (-0.5), newVar), (Eye m (-1), z1)] ]
      vecB = [Ones m (-0.5), Ones m (-0.5)]
      kones = [SOCelem [z0, z1, var x]]

  -- quad_over_lin x y = x^Tx / y, if y is vector, it's x.^2 ./ y
  scoop_quad_over_lin :: Expr -> Expr -> String -> Expr
  scoop_quad_over_lin (None s) _ _ = None s
  scoop_quad_over_lin _ (None s) _ = None s
  scoop_quad_over_lin x y s
    | isVector x && isVector y && compatible = expression newVar curvature Positive prog
    | otherwise = none $ "quad_over_lin: " ++ (name y) ++ " is not scalar or compatible with " ++ (name x)
    where
      curvature = applyDCP c1 Decreasing (vexity y)
      c1 = applyDCP Convex monotonicity (vexity x)
      compatible = (rows x == rows y) || (rows y == 1)
      monotonicity = case (sign x) of
        Positive -> Increasing
        Negative -> Decreasing
        otherwise -> Nonmonotone
      prog = (ConicSet matA vecB kones) <++> (cones x) <++> (cones y)
      (m,n) = (rows y, cols y)
      newVar = Var ("t"++s) (m, n)
      z0 = Var (name newVar ++ "z0") (m, n)
      z1 = Var (name newVar ++ "z1") (m, n)
      matA = [ Row [(Eye m 0.5, var y), (Eye m 0.5, newVar), (Eye m (-1), z0)],
               Row [(Eye m 0.5, var y), (Eye m (-0.5), newVar), (Eye m (-1), z1)] ]
      vecB = [Ones m 0, Ones m 0]
      kones
        | isScalar y = [SOC [z0,z1,var x], SOCelem [var y]]
        | otherwise = [SOCelem [z0,z1, var x], SOCelem [var y]]

  -- inv_pos(x) = 1/x for x >= 0
  scoop_inv_pos :: Expr -> String -> Expr
  scoop_inv_pos (None s) _ = None s
  scoop_inv_pos x s = expression newVar curvature Positive prog
    where
      curvature = applyDCP Convex Decreasing (vexity x)
      prog = (ConicSet matA vecB kones) <++> (cones x)
      (m,n) = (rows x, cols x)
      newVar = Var ("t"++s) (m, n)
      z0 = Var (name newVar ++ "z0") (m, n)
      z1 = Var (name newVar ++ "z1") (m, n)
      one = Var (name newVar ++ "z2") (m, n) -- has to be vector for the SOC code to work (XXX/TODO: allow SOCelem with scalars)
      matA = [ Row [(Eye m 0.5, var x), (Eye m 0.5, newVar), (Eye m (-1), z0)],
               Row [(Eye m 0.5, var x), (Eye m (-0.5), newVar), (Eye m (-1), z1)],
               Row [(Eye m 1, one)] ]
      vecB = [Ones m 0, Ones m 0, Ones m 1]
      kones = [SOCelem [z0, z1, one], SOCelem [var x]]

  -- mult a x = ax
  scoop_mult :: Expr -> Expr -> String -> Expr
  scoop_mult (None s) _ _ = None s
  scoop_mult _ (None s) _ = None s
  scoop_mult (Parameter p psgn shape) x s
    | (pm == 1) && (pn == 1) && isVector x = expression newVar curvature sgn prog
    | (pm >= 1) && (pn >= 1) && isVector x && compatible = expression newVar curvature sgn prog
    | otherwise = none $ "mult: size of " ++ (name p) ++ " and " ++ (name x) ++ " don't match"
    where
      curvature = applyDCP Affine monotonicity (vexity x)
      monotonicity = case (psgn) of
        Positive -> Increasing
        Negative -> Decreasing
        otherwise -> Nonmonotone
      sgn = psgn |*| (sign x)
      compatible = pn == rows x
      prog = (ConicSet matA vecB []) <++> (cones x)
      (pm,pn)
        | shape == NoMod = (rows p, cols p)
        | shape == Transposed = (cols p, rows p)
        | shape == Diagonal = (rows p, rows p)
      (m,n)
        | pm == 1 && pn == 1 = (rows x, cols x)
        | otherwise = (pm, cols x)
      newVar = Var ("t"++s) (m, n)
      matA
        | pm == 1 && pn == 1 = [ Row [(Diag m p, var x), (Eye m (-1), newVar)] ]
        | shape == Diagonal = [ Row [(Diag m p, var x), (Eye m (-1), newVar)] ]
        | shape == Transposed = [ Row [(MatrixT p, var x), (Eye m (-1), newVar)] ]
        | shape == NoMod = [ Row [(Matrix p, var x), (Eye m (-1), newVar)] ]
      vecB = [Ones m 0]
  scoop_mult _ _ _ = None "mult: lhs ought to be parameter"

  -- plus x y = x + y
  scoop_plus :: Expr -> Expr -> String -> Expr
  scoop_plus (None s) _ _ = None s
  scoop_plus _ (None s) _ = None s
  scoop_plus x y s
    | isVector x && isVector y && compatible = expression newVar curvature sgn prog
    | otherwise = none $ "plus: size of " ++ (name x) ++ " and " ++ (name y) ++ " don't match"
    where
      curvature = applyDCP c1 Increasing (vexity y)
      c1 = applyDCP Affine Increasing (vexity x)
      sgn = (sign x) |+| (sign y)
      compatible = (cols x == cols y) || (cols x == 1) || (cols y == 1) 
      prog = (ConicSet matA vecB []) <++> (cones x) <++> (cones y)
      (m,n) 
        | isScalar x = (rows y, cols y)
        | otherwise = (rows x, cols x)
      newVar = Var ("t"++s) (m, n)
      matA 
        | isScalar x = [Row [(Ones m 1, var x), (Eye m 1, var y), (Eye m (-1), newVar)]]
        | isScalar y = [Row [(Ones m 1, var y), (Eye m 1, var x), (Eye m (-1), newVar)]]
        | otherwise = [Row [(Eye m 1, var x), (Eye m 1, var y), (Eye m (-1), newVar)] ]
      vecB = [Ones m 0]

  -- minus x y = x - y
  scoop_minus :: Expr -> Expr -> String -> Expr
  scoop_minus (None s) _ _ = None s
  scoop_minus _ (None s) _ = None s
  scoop_minus x y s
    | isVector x && isVector y && compatible = expression newVar curvature sgn prog
    | otherwise = none $ "minus: size of " ++ (name x) ++ " and " ++ (name y) ++ " don't match"
    where
      curvature = applyDCP c1 Decreasing (vexity y)
      c1 = applyDCP Affine Increasing (vexity x)
      sgn = (sign x) |+| neg (sign y)
      compatible = (cols x == cols y) || (cols x == 1) || (cols y == 1) 
      prog = (ConicSet matA vecB []) <++> (cones x) <++> (cones y)
      (m,n) 
        | isScalar x = (rows y, cols y)
        | otherwise = (rows x, cols x)
      newVar = Var ("t"++s) (m, n)
      matA 
        | isScalar x = [Row [(Ones m 1, var x), (Eye m (-1), var y), (Eye m (-1), newVar)]]
        | isScalar y = [Row [(Ones m (-1), var y), (Eye m 1, var x), (Eye m (-1), newVar)]]
        | otherwise = [Row [(Eye m 1, var x), (Eye m (-1), var y), (Eye m (-1), newVar)] ]
      vecB = [Ones m 0]

  -- neg x = -x
  scoop_negate :: Expr -> String -> Expr
  scoop_negate (None s) _ = None s
  scoop_negate x s = expression newVar curvature sgn prog
    where
      curvature = applyDCP Affine Decreasing (vexity x)
      sgn = neg (sign x)
      prog = (ConicSet matA vecB []) <++> (cones x)
      (m,n) = (rows x, cols x)
      newVar = Var ("t"++s) (m, n)
      matA = [Row [(Eye m (-1), var x), (Eye m (-1), newVar)] ]
      vecB = [Ones m 0]

  -- pos(x) = max(x,0)
  scoop_pos :: Expr -> String -> Expr
  scoop_pos (None s) _ = None s
  scoop_pos x s = expression newVar curvature Positive prog
    where
      curvature = applyDCP Convex Increasing (vexity x)
      prog = (ConicSet matA vecB kones) <++> (cones x)
      (m,n) = (rows x, cols x)
      newVar = Var ("t"++s) (m, n)
      z0 = Var (name newVar ++ "z0") (m, n)
      matA = [Row [(Eye m (-1), var x), (Eye m 1, newVar), (Eye m (-1), z0)] ]
      vecB = [Ones m 0]
      kones = [SOCelem [newVar], SOCelem [z0]]

  -- neg(x) = max(-x,0)
  scoop_neg :: Expr -> String -> Expr
  scoop_neg (None s) _ = None s
  scoop_neg x s = expression newVar curvature Positive prog
    where
      curvature = applyDCP Convex Decreasing (vexity x)
      prog = (ConicSet matA vecB kones) <++> (cones x)
      (m,n) = (rows x, cols x)
      newVar = Var ("t"++s) (m, n)
      z0 = Var (name newVar ++ "z0") (m, n)
      matA = [Row [(Eye m 1, var x), (Eye m 1, newVar), (Eye m (-1), z0)] ]
      vecB = [Ones m 0]
      kones = [SOCelem [newVar], SOCelem [z0]]
  
  -- max(x) = max(x_1, x_2, \ldots, x_n)
  scoop_max :: Expr -> String -> Expr
  scoop_max (None s) _ = None s
  scoop_max x s = expression newVar curvature (sign x) prog
    where
      curvature = applyDCP Convex Increasing (vexity x)
      prog = (ConicSet matA vecB kones) <++> (cones x)
      (m, n) = (rows x, cols x)
      newVar = Var ("t"++s) (1, 1)
      z0 = Var (name newVar ++ "z0") (m, n)
      matA = [Row [(Ones m 1, newVar), (Eye m (-1), var x), (Eye m (-1), z0)]]
      vecB = [Ones m 0]
      kones = [SOCelem [z0]]


  -- min(x) = min (x_1, x_2, \ldots, x_n)
  scoop_min :: Expr -> String -> Expr
  scoop_min (None s) _ = None s
  scoop_min x s = expression newVar curvature (sign x) prog
    where
      curvature = applyDCP Concave Increasing (vexity x)
      prog = (ConicSet matA vecB kones) <++> (cones x)
      (m, n) = (rows x, cols x)
      newVar = Var ("t"++s) (1, 1)
      z0 = Var (name newVar ++ "z0") (m, n)
      matA = [Row [(Ones m (-1), newVar), (Eye m 1, var x), (Eye m (-1), z0)]] -- x - z0 == t, z0 >= 0
      vecB = [Ones m 0]
      kones = [SOCelem [z0]]

  -- min x =>
  -- minimize t
  -- x - z == t*ONES, z >= 0
  -- min x,y =>
  -- minimize t
  -- x - z0 == t, y - z1 == t, z0, z1 >= 0, (t is a vector)
 
  -- sum(x) = x_1 + x_2 + ... + x_n
  scoop_sum :: Expr -> String -> Expr
  scoop_sum (None s) _ = None s
  scoop_sum x s = expression newVar curvature (sign x) prog
    where      
      curvature = applyDCP Affine Increasing (vexity x)
      prog = (ConicSet matA vecB []) <++> (cones x)
      m = rows x
      newVar = Var ("t"++s) (1, 1)
      matA = [Row [(Ones 1 (-1), newVar), (OnesT m 1, var x)]]
      vecB = [Ones 1 0]

  -- sum x = 1^T x
  -- sum x, y = x + y

  -- abs(x) = |x|
  scoop_abs :: Expr -> String -> Expr
  scoop_abs (None s) _ = None s
  scoop_abs x s = expression newVar curvature Positive prog
    where
      curvature = applyDCP Convex monotonicity (vexity x)
      monotonicity = case (sign x) of
        Positive -> Increasing
        Negative -> Decreasing
        otherwise -> Nonmonotone
      prog = (ConicSet [] [] kones) <++> (cones x)
      (m,n) = (rows x, cols x)
      newVar = Var ("t"++s) (m, n)
      kones = [SOCelem [newVar, var x]]


  -- norm(x) = ||x||_2
  scoop_norm :: Expr -> String -> Expr
  scoop_norm (None s) _ = None s
  scoop_norm x s = expression newVar curvature Positive prog
    where
      curvature = applyDCP Convex monotonicity (vexity x)
      monotonicity = case (sign x) of
        Positive -> Increasing
        Negative -> Decreasing
        otherwise -> Nonmonotone
      prog = (ConicSet [] [] kones) <++> (cones x)
      newVar = Var ("t"++s) (1, 1)
      kones = [SOC [newVar, var x]]

  -- norm_inf(x) = ||x||_\infty
  scoop_norm_inf x s = scoop_max (scoop_abs x (s++"s0")) s

  -- norm1(x) = ||x||_1
  scoop_norm1 x s = scoop_sum (scoop_abs x (s++"s0")) s

  -- sqrt(x) = geo_mean(x,1)
  scoop_sqrt :: Expr -> String -> Expr
  scoop_sqrt (None s) _ = None s
  scoop_sqrt x s = expression newVar curvature Positive prog
    where
      curvature = applyDCP Concave Increasing (vexity x)
      prog = (ConicSet matA vecB kones) <++> (cones x)
      (m,n) = (rows x, cols x)
      newVar = Var ("t"++s) (m,n)
      z0 = Var (name newVar ++ "z0") (m,n)
      z1 = Var (name newVar ++ "z1") (m,n)
      matA = [Row [(Eye m 0.5, var x), (Eye m (-1), z0)],
              Row [(Eye m (-0.5), var x), (Eye m (-1), z1)]]
      vecB = [Ones m (-0.5), Ones m (-0.5)]
      kones = [SOCelem [z0,z1,newVar]]
      
  -- geo_mean(x,y) = sqrt(x*y)
  scoop_geo_mean :: Expr -> Expr -> String -> Expr
  scoop_geo_mean (None s) _ _ = None s
  scoop_geo_mean _ (None s) _ = None s
  scoop_geo_mean x y s
    | isVector x && isVector y && compatible = expression newVar curvature Positive prog
    | otherwise = none $ "geo_mean: " ++ (name x) ++ " and " ++ (name y) ++ " are not of compatible dimensions"
    where
      curvature = applyDCP c1 Increasing (vexity y)
      c1 = applyDCP Concave Increasing (vexity x)
      compatible = (cols x == cols y) || (cols x == 1) || (cols y == 1) 
      prog = (ConicSet matA vecB kones) <++> (cones x) <++> (cones y)
      (m,n) 
        | isScalar x = (rows y, cols y)
        | otherwise = (rows x, cols x)
      newVar = Var ("t"++s) (m,n)
      z0 = Var (name newVar ++ "z0") (m,n)
      z1 = Var (name newVar ++ "z1") (m,n)
      matA
        | isScalar x = [Row [(Ones m 0.5, var x), (Eye m 0.5, var y), (Eye m (-1), z0)],
                        Row [(Ones m (-0.5), var x), (Eye m 0.5, var y), (Eye m (-1), z1)]]
        | isScalar y = [Row [(Eye m 0.5, var x), (Ones m 0.5, var y), (Eye m (-1), z0)],
                        Row [(Eye m (-0.5), var x), (Ones m 0.5, var y), (Eye m (-1), z1)]]
        | otherwise = [Row [(Eye m 0.5, var x), (Eye m 0.5, var y), (Eye m (-1), z0)],
                       Row [(Eye m (-0.5), var x), (Eye m 0.5, var y), (Eye m (-1), z1)]]
      vecB = [Ones m 0, Ones m 0]
      kones = [SOCelem [z0, z1, newVar], SOCelem [var y]]

  -- pow_rat (x, p,q) = x^(p/q) for q <= p <= 4
  scoop_pow_rat :: Expr -> Integer -> Integer -> String -> Expr
  scoop_pow_rat x 4 3 s = result -- also tacks on constraint that x >= 0
    where curvature = applyDCP Convex Increasing (vexity x)
          newVar = Var ("t"++s) (rows x, cols x)
          result = case (scoop_geq (scoop_pow_rat (Variable newVar) 3 4 (s++"s0")) x (s++"s1")) of
            Just kone -> expression newVar curvature Positive (kone <++> (ConicSet [] [] [SOCelem [var x]]))
            Nothing -> none $ "unknown x^(4/3) error?"
  scoop_pow_rat x 4 2 s = scoop_square x s
  scoop_pow_rat x 4 1 s = scoop_square (scoop_square x s) (s++"s0")
  scoop_pow_rat x 3 2 s = scoop_quad_over_lin x (scoop_sqrt x s) (s++"s0")
  scoop_pow_rat x 3 1 s = scoop_quad_over_lin (scoop_square x s) x (s++"s0")
  scoop_pow_rat x 2 1 s = scoop_square x s
  scoop_pow_rat x 1 2 s = scoop_sqrt x s
  scoop_pow_rat x 1 3 s = result
    where curvature = applyDCP Concave Increasing (vexity x)
          newVar = Var ("t"++s) (rows x, cols x)
          result = case (scoop_leq (scoop_pow_rat (Variable newVar) 3 1 (s++"s0")) x (s++"s1")) of
            Just kone -> expression newVar curvature Positive (kone <++> (ConicSet [] [] [SOCelem [var x]]))
            Nothing -> none $ "unknown x^(1/3) error?"
  scoop_pow_rat x 2 3 s = result
    where curvature = applyDCP Concave Increasing (vexity x)
          newVar = Var ("t"++s) (rows x, cols x)
          result = case (scoop_leq (scoop_pow_rat (Variable newVar) 3 2 (s++"s0")) x (s++"s1")) of
            Just kone -> expression newVar curvature Positive (kone <++> (ConicSet [] [] [SOCelem [var x]]))
            Nothing -> none $ "unknown x^(2/3) error?"
  scoop_pow_rat x 1 4 s = scoop_sqrt (scoop_sqrt x s) (s++"s0")
  scoop_pow_rat x 2 4 s = scoop_sqrt x s
  scoop_pow_rat x 3 4 s = scoop_geo_mean x (scoop_sqrt x s) (s++"s0")
  scoop_pow_rat x p q _ 
    | p == q = x
    | otherwise = None $ "pow_rat: not implemented for p = " ++ show p ++ " and q = " ++ show q

  --   -- sum_largest(x,k) <-- also not implemented (uses LP dual)
  --   

--}
