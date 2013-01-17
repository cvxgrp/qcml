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
  scoop_eq, scoop_leq, scoop_geq, 
  scoop_concat, scoop_transpose, scoop_diag,
  scoop_square, scoop_quad_over_lin, scoop_inv_pos, scoop_pos, scoop_neg,
  scoop_max, scoop_min,
  scoop_sum, scoop_abs, scoop_norm, scoop_norm1, scoop_norm_inf,
  scoop_sqrt, scoop_geo_mean,
  module Atoms.Common) where
  import Expression.Expression
  import Atoms.Common
  import Data.List (intercalate)

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

  -- assumes size / dimension checking will be done at code generation stage
  -- a "_variable" is a private type for which dimension checking is inferred (done later)

  primitiveAdd :: Expr -> Expr -> Rewriter Expr
  primitiveAdd x y = do
    t <- newVar

    let v = Affine <&> increasing x <&> increasing y
    let s = case (sign x, sign y) of
              (Positive, Positive) -> Positive
              (Negative, Negative) -> Negative
              otherwise -> Unknown
    -- definition of plus
    addLine $ concat [t, " == ", (name x), " + ", (name y)]
    return $ Expr t v s

  primitiveMinus :: Expr -> Expr -> Rewriter Expr
  primitiveMinus x y = do
    t <- newVar

    let v = Affine <&> increasing x <&> decreasing y
    let s = case (sign x, sign y) of
              (Positive, Negative) -> Positive
              (Negative, Positive) -> Negative
              otherwise -> Unknown
    -- definition of plus
    addLine $ concat [t, " == ", (name x), " - ", (name y)]
    return $ Expr t v s

  primitiveNegate :: Expr -> Rewriter Expr
  primitiveNegate x = do
    t <- newVar

    let v = Affine <&> decreasing x
    let s = case (sign x) of
              Positive -> Negative
              Negative -> Positive
              otherwise -> Unknown

    addLine $ concat [t, " == -", (name x)]
    return $ Expr t v s

  primitiveMultiply :: Param -> Expr -> Rewriter Expr
  primitiveMultiply p x = do
    t <- newVar

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
    return $ Expr t v s

  -- operations for creating statements
  scoop_leq :: Symbol -> Symbol -> Rewriter ()
  scoop_leq x y = do
    if(isConvex x && isConcave y) then do
      slack <- newVar
      addLine $ concat [name x, " + ", slack, " == ", name y]
      addLine $ slack ++ " >= 0"
    else
      fail $ "Nonconvex inequality constraint (" ++ show (vexity x) ++ " <= " ++ show (vexity y) ++ ")."

  scoop_geq :: Symbol -> Symbol -> Rewriter ()
  scoop_geq x y = do
    if (isConcave x && isConvex y) then do
      slack <- newVar
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

  -- going with alternative 2, introduced "_variable" keyword

  -- operations that create Expr
  -- t = [x;y;z; ...]
  --    concatenation will be code generator operation
  scoop_concat :: [Expr] -> Rewriter Expr
  scoop_concat xs = do
    let v = foldr (\y vex -> vex <&> increasing y) Affine xs
    let s | all (==Positive) (map sign xs) = Positive
          | all (==Negative) (map sign xs) = Negative
          | otherwise = Unknown
    t <- newVar
    addLine $ (t ++ " ==[ " ++ intercalate "; " (map name xs) ++ " ]")
    return $ Expr t v s
  -- operators on parameters

  -- transpose a = a' (new parameter named " a' ")
  -- this works perfectly in Matlab, but care must be taken at
  -- codegen to parse a "tick" as a transposed parameter
  scoop_transpose :: Param -> Rewriter Param
  scoop_transpose x = return $ Param ((name x)++"\'") (sign x)

  -- diag a = diag(a) (new diagonal matrix parameter), handled at codegen
  -- TODO: check that Param is indeed a vector param
  scoop_diag :: Param -> Rewriter Param
  scoop_diag x = return $ Param ("diag(" ++ name x ++ ")") (sign x)

  -- TODO: atoms!
  -- TODO: DCP error messages are part of rewriter
  -- TODO: are type errors a part of rewriter?



-- square x = x^2
  scoop_square :: Expr -> Rewriter Expr
  scoop_square x = do
    let v = case (sign x) of
              Positive -> Convex <&> increasing x
              Negative -> Convex <&> decreasing x
              otherwise -> Convex <&> nonmonotone x
      
    t <- newVar
    z0 <- newVar
    z1 <- newVar
    
    addLine $ "0.5*" ++ t ++ " - " ++ z0 ++ " == -0.5"
    addLine $ "-0.5*" ++ t ++ " - " ++ z1 ++ " == -0.5"
    addLine $ "norm(" ++ z1 ++ ", " ++ (name x) ++ ") <= " ++ z0
      
    return $ Expr t v Positive

  -- quad_over_lin x y = x^Tx / y, if y is vector, it's x.^2 ./ y
  scoop_quad_over_lin :: Expr -> Expr -> Rewriter Expr
  scoop_quad_over_lin x y = do
    let v = case (sign x) of
              Positive -> Convex <&> increasing x <&> decreasing y
              Negative -> Convex <&> decreasing x <&> decreasing y
              otherwise -> Convex <&> nonmonotone x <&> decreasing y
    
    t <- newVar
    z0 <- newVar
    z1 <- newVar
    
    addLine $ concat ["0.5*", name y, " + 0.5*", t, " - ", z0, " == 0"]
    addLine $ concat ["0.5*", name y, " - 0.5*", t, " - ", z1, " == 0"]
    addLine $ (name y) ++ " >= 0"
    addLine $ concat ["norm([", z1, "; ", name x, "]) <= ", z0]
    
    return $ Expr t v Positive
      

  -- inv_pos(x) = 1/x for x >= 0
  scoop_inv_pos :: Expr -> Rewriter Expr
  scoop_inv_pos x = do
    let v = Convex <&> decreasing x
    
    t <- newVar
    z0 <- newVar
    z1 <- newVar
    one <- newVar
    
    addLine $ concat ["0.5*", name x, " + 0.5*", t, " - ", z0, " == 0"]
    addLine $ concat ["0.5*", name x, " - 0.5*", t, " - ", z1, " == 0"]
    addLine $ concat [one, " == 1"]
    addLine $ concat [name x, " >= 0"]
    addLine $ concat ["norm(", z1, ", ", one, ") <= ", z0]
    
    return $ Expr t v Positive


  -- pos(x) = max(x,0)
  scoop_pos :: Expr -> Rewriter Expr
  scoop_pos x = do
    let v = Convex <&> increasing x
    t <- newVar
    z0 <- newVar
    
    addLine $ concat [t, " - ", name x, " - ", z0, " == 0"]
    addLine (t ++ " >= 0")
    addLine (z0 ++ " >= 0")
    
    return $ Expr t v Positive

  -- neg(x) = max(-x,0)
  scoop_neg :: Expr -> Rewriter Expr
  scoop_neg x = do
    let v = Convex <&> decreasing x
    t <- newVar
    z0 <- newVar
    
    addLine $ concat [t, " + ", name x, " - ", z0, " == 0"]
    addLine (t ++ " >= 0")
    addLine (z0 ++ " >= 0")
    
    return $ Expr t v Positive
  
  -- max(x) = max(x_1, x_2, \ldots, x_n)
  scoop_max :: Expr -> Rewriter Expr
  scoop_max x = do
    let v = Convex <&> increasing x
    
    t <- newVar
    z0 <- newVar
    
    addLine $ concat ["ones*", t, " - ", name x, " - ", z0, " == 0"]
    addLine (z0 ++ " >= 0")
    
    return $ Expr t v (sign x)


  -- min(x) = min (x_1, x_2, \ldots, x_n)
  scoop_min :: Expr -> Rewriter Expr
  scoop_min x = do
    let v = Concave <&> increasing x
    
    t <- newVar
    z0 <- newVar
    
    addLine $ concat [name x, " - ones*", t, " - ", z0, " == 0"]
    addLine (z0 ++ " >= 0")
    
    return $ Expr t v (sign x)
    
  -- min x =>
  -- minimize t
  -- x - z == t*ONES, z >= 0
  -- min x,y =>
  -- minimize t
  -- x - z0 == t, y - z1 == t, z0, z1 >= 0, (t is a vector)
 
  -- sum(x) = x_1 + x_2 + ... + x_n
  scoop_sum :: Expr -> Rewriter Expr
  scoop_sum x = do
    let v = Affine <&> increasing x
    t <- newVar
    
    addLine $ concat ["ones'*", (name x), " - ", t, " == 0"]
    
    return $ Expr t v (sign x)

  -- sum x = 1^T x
  -- sum x, y = x + y

  -- abs(x) = |x|
  scoop_abs :: Expr -> Rewriter Expr
  scoop_abs x = do
    let v = case (sign x) of 
              Positive -> Convex <&> increasing x
              Negative -> Convex <&> decreasing x
              otherwise -> Convex <&> nonmonotone x
    
    t <- newVar
    
    addLine $ concat ["norm(", name x, ") <= ", t, " # this is elementwise..."]
    
    return $ Expr t v Positive
     
    -- expression newVar curvature Positive prog
    -- where
    --   curvature = applyDCP Convex monotonicity (vexity x)
    --   monotonicity = case (sign x) of
    --     Positive -> Increasing
    --     Negative -> Decreasing
    --     otherwise -> Nonmonotone
    --   prog = (ConicSet [] [] kones) <++> (cones x)
    --   (m,n) = (rows x, cols x)
    --   newVar = Var ("t"++s) (m, n)
    --   kones = [SOCelem [newVar, var x]]


  -- norm(x) = ||x||_2
  scoop_norm :: Expr -> Rewriter Expr
  scoop_norm x = do
    let v = case (sign x) of 
              Positive -> Convex <&> increasing x
              Negative -> Convex <&> decreasing x
              otherwise -> Convex <&> nonmonotone x
      
    t <- newVar
    
    addLine $ concat ["norm(", name x, ") <= ", t]
    
    return $ Expr t v Positive

  -- norm_inf(x) = ||x||_\infty
  scoop_norm_inf x = do
    t <- scoop_abs x
    scoop_max t

  -- norm1(x) = ||x||_1
  scoop_norm1 x = do
    t <- scoop_abs x
    scoop_sum t

  -- sqrt(x) = geo_mean(x,1)
  scoop_sqrt :: Expr -> Rewriter Expr
  scoop_sqrt x = do
    let v = Concave <&> increasing x
    t <- newVar
    z0 <- newVar
    z1 <- newVar
    
    addLine $ concat ["0.5*", name x, " - ", z0, " == -0.5"]
    addLine $ concat ["-0.5*", name x, " - ", z1, " == -0.5"]
    addLine $ concat ["norm(", z0, ", ", z1, ") <= ", t]

    return $ Expr t v Positive
      
  -- geo_mean(x,y) = sqrt(x*y)
  scoop_geo_mean :: Expr -> Expr -> Rewriter Expr
  scoop_geo_mean x y = do
    let v = Concave <&> increasing x <&> increasing y
    
    t <- newVar
    z0 <- newVar
    z1 <- newVar
    
    addLine $ concat ["0.5*", name x, " + 0.5*", name y, " - ", z0, " == 0"]
    addLine $ concat ["-0.5*", name x, " + 0.5*", name y, " - ", z1, " == 0"]
    addLine $ concat ["norm(", z0, ", ", z1, ") <= ", t]
    addLine $ (name y) ++ " <= 0"
    
    return $ Expr t v Positive
    
    -- | isVector x && isVector y && compatible = expression newVar curvature Positive prog
    -- | otherwise = none $ "geo_mean: " ++ (name x) ++ " and " ++ (name y) ++ " are not of compatible dimensions"
    -- where
    --   curvature = applyDCP c1 Increasing (vexity y)
    --   c1 = applyDCP Concave Increasing (vexity x)
    --   compatible = (cols x == cols y) || (cols x == 1) || (cols y == 1) 
    --   prog = (ConicSet matA vecB kones) <++> (cones x) <++> (cones y)
    --   (m,n) 
    --     | isScalar x = (rows y, cols y)
    --     | otherwise = (rows x, cols x)
    --   newVar = Var ("t"++s) (m,n)
    --   z0 = Var (name newVar ++ "z0") (m,n)
    --   z1 = Var (name newVar ++ "z1") (m,n)
    --   matA
    --     | isScalar x = [Row [(Ones m 0.5, var x), (Eye m 0.5, var y), (Eye m (-1), z0)],
    --                     Row [(Ones m (-0.5), var x), (Eye m 0.5, var y), (Eye m (-1), z1)]]
    --     | isScalar y = [Row [(Eye m 0.5, var x), (Ones m 0.5, var y), (Eye m (-1), z0)],
    --                     Row [(Eye m (-0.5), var x), (Ones m 0.5, var y), (Eye m (-1), z1)]]
    --     | otherwise = [Row [(Eye m 0.5, var x), (Eye m 0.5, var y), (Eye m (-1), z0)],
    --                    Row [(Eye m (-0.5), var x), (Eye m 0.5, var y), (Eye m (-1), z1)]]
    --   vecB = [Ones m 0, Ones m 0]
    --   kones = [SOCelem [z0, z1, newVar], SOCelem [var y]]

{--
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
