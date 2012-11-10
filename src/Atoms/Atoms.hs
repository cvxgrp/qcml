module Atoms.Atoms(
  ecos_square,
  ecos_quad_over_lin,
  ecos_inv_pos,
  ecos_mult,
  ecos_plus,
  ecos_minus,
  ecos_negate,
  ecos_max,
  ecos_min,
  ecos_pos,
  ecos_neg,
  ecos_sum,
  ecos_norm,
  ecos_abs,
  ecos_norm_inf,
  ecos_norm1,
  ecos_sqrt,
  ecos_geo_mean,
  ecos_concat,
  ecos_transpose,
  ecos_eq,
  ecos_geq,
  ecos_leq,
  ecos_pow_rat,
  isScalar,
  isVector,
  isMatrix,
  isConvex,
  isConcave,
  isAffine
) where
  
  -- semantics:
  -- for scalar func of vector is applied elementwise
  -- for vector func of arg *list* is applied *row-wise*

  -- TODO: would like coeffs to automatically "scale" based on their input and output sizes?
  --        e.g., "Id m 1" coeff would return ones(m,1), "Id m m" would return eye(m), "Id m n" would return... ?
  -- TODO: constant folding
  -- TODO: creating new variables is a bit of a pain, maybe make a factory? put atoms in a monad?
  -- TODO: slicing (important for handling quad_over_lin)
  import Expression.Expression
  import Data.Tuple(swap)

  -- helper functions for guards
  isVector :: Expr -> Bool
  isVector x = (rows x) >= 1 && (cols x) == 1

  isScalar :: Expr -> Bool
  isScalar x = (rows x) == 1 && (cols x) == 1

  isMatrix :: Expr -> Bool
  isMatrix x = (rows x) >= 1 && (cols x) >= 1

  isConvex :: Expr -> Bool
  isConvex x
    | vexity x == Convex = True
    | vexity x == Affine = True
    | otherwise = False

  isConcave :: Expr -> Bool
  isConcave x
    | vexity x == Concave = True
    | vexity x == Affine = True
    | otherwise = False

  isAffine x = isConcave x && isConvex x

  -- sign operations

  -- how to *multiply* two signs
  (<*>) :: Sign -> Sign -> Sign
  Positive <*> Positive = Positive
  Negative <*> Negative = Positive
  Positive <*> Negative = Negative
  Negative <*> Positive = Negative
  _ <*> _ = Unknown

  -- how to *add* two signs
  (<+>) :: Sign -> Sign -> Sign
  Positive <+> Positive = Positive
  Negative <+> Negative = Negative
  _ <+> _ = Unknown

  -- how to *negate* a sign
  neg :: Sign -> Sign
  neg Positive = Negative
  neg Negative = Positive
  neg _ = Unknown

  -- constructors to promote scalars?


  -- begin list of atoms
  -- in addition to arguments, atoms take a string to uniquely identify/modify their variables
  
  -- square x = x^2
  ecos_square :: Expr -> String -> Expr
  ecos_square (None s) _ = None s
  ecos_square x s = expression newVar curvature Positive prog
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
  ecos_quad_over_lin :: Expr -> Expr -> String -> Expr
  ecos_quad_over_lin (None s) _ _ = None s
  ecos_quad_over_lin _ (None s) _ = None s
  ecos_quad_over_lin x y s
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
  ecos_inv_pos :: Expr -> String -> Expr
  ecos_inv_pos (None s) _ = None s
  ecos_inv_pos x s = expression newVar curvature Positive prog
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
  ecos_mult :: Expr -> Expr -> String -> Expr
  ecos_mult (None s) _ _ = None s
  ecos_mult _ (None s) _ = None s
  ecos_mult (Parameter p psgn) x s
    | (pm == 1) && (pn == 1) && isVector x = expression newVar curvature sgn prog
    | (pm >= 1) && (pn >= 1) && isVector x && compatible = expression newVar curvature sgn prog
    | otherwise = none $ "mult: size of " ++ (name p) ++ " and " ++ (name x) ++ " don't match"
    where
      curvature = applyDCP Affine monotonicity (vexity x)
      monotonicity = case (psgn) of
        Positive -> Increasing
        Negative -> Decreasing
        otherwise -> Nonmonotone
      sgn = psgn <*> (sign x)
      compatible = pn == rows x
      prog = (ConicSet matA vecB []) <++> (cones x)
      (pm,pn) = (rows p, cols p)
      (m,n)
        | pm == 1 && pn == 1 = (rows x, cols x)
        | otherwise = (pm, cols x)
      newVar = Var ("t"++s) (m, n)
      matA
        | pm == 1 && pn == 1 = [ Row [(Diag m p, var x), (Eye m (-1), newVar)] ]
        | otherwise = [ Row [(Matrix p, var x), (Eye m (-1), newVar)] ]
      vecB = [Ones m 0]
  ecos_mult _ _ _ = None "mult: lhs ought to be parameter"

  -- plus x y = x + y
  ecos_plus :: Expr -> Expr -> String -> Expr
  ecos_plus (None s) _ _ = None s
  ecos_plus _ (None s) _ = None s
  ecos_plus x y s
    | isVector x && isVector y && compatible = expression newVar curvature sgn prog
    | otherwise = none $ "plus: size of " ++ (name x) ++ " and " ++ (name y) ++ " don't match"
    where
      curvature = applyDCP c1 Increasing (vexity y)
      c1 = applyDCP Affine Increasing (vexity x)
      sgn = (sign x) <+> (sign y)
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
  ecos_minus :: Expr -> Expr -> String -> Expr
  ecos_minus (None s) _ _ = None s
  ecos_minus _ (None s) _ = None s
  ecos_minus x y s
    | isVector x && isVector y && compatible = expression newVar curvature sgn prog
    | otherwise = none $ "minus: size of " ++ (name x) ++ " and " ++ (name y) ++ " don't match"
    where
      curvature = applyDCP c1 Decreasing (vexity y)
      c1 = applyDCP Affine Increasing (vexity x)
      sgn = (sign x) <+> neg (sign y)
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
  ecos_negate :: Expr -> String -> Expr
  ecos_negate (None s) _ = None s
  ecos_negate x s = expression newVar curvature sgn prog
    where
      curvature = applyDCP Affine Decreasing (vexity x)
      sgn = neg (sign x)
      prog = (ConicSet matA vecB []) <++> (cones x)
      (m,n) = (rows x, cols x)
      newVar = Var ("t"++s) (m, n)
      matA = [Row [(Eye m (-1), var x), (Eye m (-1), newVar)] ]
      vecB = [Ones m 0]

  -- pos(x) = max(x,0)
  ecos_pos :: Expr -> String -> Expr
  ecos_pos (None s) _ = None s
  ecos_pos x s = expression newVar curvature Positive prog
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
  ecos_neg :: Expr -> String -> Expr
  ecos_neg (None s) _ = None s
  ecos_neg x s = expression newVar curvature Positive prog
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
  ecos_max :: Expr -> String -> Expr
  ecos_max (None s) _ = None s
  ecos_max x s = expression newVar curvature (sign x) prog
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
  ecos_min :: Expr -> String -> Expr
  ecos_min (None s) _ = None s
  ecos_min x s = expression newVar curvature (sign x) prog
    where
      curvature = applyDCP Concave Increasing (vexity x)
      prog = (ConicSet matA vecB kones) <++> (cones x)
      (m, n) = (rows x, cols x)
      newVar = Var ("t"++s) (1, 1)
      z0 = Var (name newVar ++ "z0") (m, n)
      matA = [Row [(Ones m (-1), newVar), (Eye m 1, var x), (Eye m (-1), z0)]]
      vecB = [Ones m 0]
      kones = [SOCelem [z0]]
 
  -- sum(x) = x_1 + x_2 + ... + x_n
  ecos_sum :: Expr -> String -> Expr
  ecos_sum (None s) _ = None s
  ecos_sum x s = expression newVar curvature (sign x) prog
    where      
      curvature = applyDCP Affine Increasing (vexity x)
      prog = (ConicSet matA vecB []) <++> (cones x)
      m = rows x
      newVar = Var ("t"++s) (1, 1)
      matA = [Row [(Ones 1 (-1), newVar), (OnesT m 1, var x)]]
      vecB = [Ones 1 0]

  -- abs(x) = |x|
  ecos_abs :: Expr -> String -> Expr
  ecos_abs (None s) _ = None s
  ecos_abs x s = expression newVar curvature Positive prog
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
  ecos_norm :: Expr -> String -> Expr
  ecos_norm (None s) _ = None s
  ecos_norm x s = expression newVar curvature Positive prog
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
  ecos_norm_inf x s = ecos_max (ecos_abs x (s++"s0")) s

  -- norm1(x) = ||x||_1
  ecos_norm1 x s = ecos_sum (ecos_abs x (s++"s0")) s

  -- sqrt(x) = geo_mean(x,1)
  ecos_sqrt :: Expr -> String -> Expr
  ecos_sqrt (None s) _ = None s
  ecos_sqrt x s = expression newVar curvature Positive prog
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
  ecos_geo_mean :: Expr -> Expr -> String -> Expr
  ecos_geo_mean (None s) _ _ = None s
  ecos_geo_mean _ (None s) _ = None s
  ecos_geo_mean x y s
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
  ecos_pow_rat :: Expr -> Integer -> Integer -> String -> Expr
  ecos_pow_rat x 4 3 s = result -- also tacks on constraint that x >= 0
    where curvature = applyDCP Convex Increasing (vexity x)
          newVar = Var ("t"++s) (rows x, cols x)
          result = case (ecos_geq (ecos_pow_rat (Variable newVar) 3 4 (s++"s0")) x (s++"s1")) of
            Just kone -> expression newVar curvature Positive (kone <++> (ConicSet [] [] [SOCelem [var x]]))
            Nothing -> none $ "unknown x^(4/3) error?"
  ecos_pow_rat x 4 2 s = ecos_square x s
  ecos_pow_rat x 4 1 s = ecos_square (ecos_square x s) (s++"s0")
  ecos_pow_rat x 3 2 s = ecos_quad_over_lin x (ecos_sqrt x s) (s++"s0")
  ecos_pow_rat x 3 1 s = ecos_quad_over_lin (ecos_square x s) x (s++"s0")
  ecos_pow_rat x 2 1 s = ecos_square x s
  ecos_pow_rat x 1 2 s = ecos_sqrt x s
  ecos_pow_rat x 1 3 s = result
    where curvature = applyDCP Concave Increasing (vexity x)
          newVar = Var ("t"++s) (rows x, cols x)
          result = case (ecos_leq (ecos_pow_rat (Variable newVar) 3 1 (s++"s0")) x (s++"s1")) of
            Just kone -> expression newVar curvature Positive (kone <++> (ConicSet [] [] [SOCelem [var x]]))
            Nothing -> none $ "unknown x^(1/3) error?"
  ecos_pow_rat x 2 3 s = result
    where curvature = applyDCP Concave Increasing (vexity x)
          newVar = Var ("t"++s) (rows x, cols x)
          result = case (ecos_leq (ecos_pow_rat (Variable newVar) 3 2 (s++"s0")) x (s++"s1")) of
            Just kone -> expression newVar curvature Positive (kone <++> (ConicSet [] [] [SOCelem [var x]]))
            Nothing -> none $ "unknown x^(2/3) error?"
  ecos_pow_rat x 1 4 s = ecos_sqrt (ecos_sqrt x s) (s++"s0")
  ecos_pow_rat x 2 4 s = ecos_sqrt x s
  ecos_pow_rat x 3 4 s = ecos_geo_mean x (ecos_sqrt x s) (s++"s0")
  ecos_pow_rat x p q _ 
    | p == q = x
    | otherwise = None $ "pow_rat: not implemented for p = " ++ show p ++ " and q = " ++ show q

  --   -- sum_largest(x,k) <-- also not implemented (uses LP dual)
  --   

  -- t = [x;y;z; ...]
  -- XXX/TODO: do we need a None here?
  ecos_concat :: [Expr] -> String -> Expr
  ecos_concat x s = expression newVar curvature sgn prog
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

  -- transpose a = a' (new parameter named " a' ")
  -- this works perfectly in Matlab, but care must be taken at
  -- codegen to parse a "tick" as a transposed parameter
  ecos_transpose :: Expr -> Expr
  ecos_transpose (None s) = None s
  ecos_transpose (Parameter p psgn) = Parameter (Param (name p) (swap $ shape p) (not $ transposed p)) psgn
  ecos_transpose x = None $ "transpose: cannot transpose " ++ (name x) ++ "; can only transpose parameters"

  -- inequalities (returns "Maybe ConicSet", since ConicSet are convex)
  -- if it's an invalid inequality, will produce Nothing
  -- a >= b
  ecos_geq :: Expr -> Expr -> String -> Maybe ConicSet
  ecos_geq a b s
    | isConvexSet && (m1 == m2) = Just prog
    | isConvexSet && (m1 == 1) = Just prog
    | isConvexSet && (m2 == 1) = Just prog
    | otherwise = Nothing 
    where prog = (ConicSet matA vecB [SOCelem [slack]]) <++> (cones a) <++> (cones b)
          (m1, n1) = (rows a, cols a)
          (m2, n2) = (rows b, cols b)
          (m, n) = (max m1 m2, max n1 n2)
          slack = Var ("t"++s) (m,n)
          coeff1
            | m1 == 1 = Ones m 1
            | otherwise = Eye m 1
          coeff2
            | m2 == 1 = Ones m (-1)
            | otherwise = Eye m (-1)
          matA = [Row [(coeff1, var a), (Eye m (-1), slack), (coeff2, var b)]]
          vecB = [Ones m 0]
          isConvexSet = (isConcave a) && (isConvex b)

  -- a <= b
  ecos_leq :: Expr -> Expr -> String -> Maybe ConicSet
  ecos_leq a b s
    | isConvexSet && (m1 == m2) = Just prog
    | isConvexSet && (m1 == 1) = Just prog
    | isConvexSet && (m2 == 1) = Just prog
    | otherwise = Nothing 
    where prog = (ConicSet matA vecB [SOCelem [slack]]) <++> (cones a) <++> (cones b)
          (m1, n1) = (rows a, cols a)
          (m2, n2) = (rows b, cols b)
          (m, n) = (max m1 m2, max n1 n2)
          slack = Var ("t"++s) (m,n)
          coeff1
            | m1 == 1 = Ones m 1
            | otherwise = Eye m 1
          coeff2
            | m2 == 1 = Ones m (-1)
            | otherwise = Eye m (-1)
          matA = [Row [(coeff1, var a), (Eye m 1, slack), (coeff2, var b)]]
          vecB = [Ones m 0]
          isConvexSet = (isConvex a) && (isConcave b)

  -- a == b
  ecos_eq :: Expr -> Expr -> Maybe ConicSet
  ecos_eq a b
    | isConvexSet && (m1 == m2) = Just prog
    | isConvexSet && (m1 == 1) = Just prog
    | isConvexSet && (m2 == 1) = Just prog
    | otherwise = Nothing 
    where prog = (ConicSet matA vecB []) <++> (cones a) <++> (cones b)
          (m1, n1) = (rows a, cols a)
          (m2, n2) = (rows b, cols b)
          (m, n) = (max m1 m2, max n1 n2)
          coeff1
            | m1 == 1 = Ones m 1
            | otherwise = Eye m 1
          coeff2
            | m2 == 1 = Ones m (-1)
            | otherwise = Eye m (-1)
          matA = [Row [(coeff1, var a), (coeff2, var b)]]
          vecB = [Ones m 0]
          isConvexSet = (isAffine a) && (isAffine b)

