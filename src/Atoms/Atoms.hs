module Atoms.Atoms(
  ecos_square,
  ecos_quad_over_lin,
  ecos_inv_pos,
  ecos_constant,
  ecos_mult,
  ecos_plus,
  ecos_minus,
  ecos_negate,
  ecos_max,
  ecos_min,
  ecos_sum,
  ecos_norm,
  ecos_norm_inf,
  ecos_norm1,
  ecos_sqrt,
  ecos_geo_mean
) where
  
  -- TODO: constant folding
  -- TODO: creating new variables is a bit of a pain, maybe make a factory?
  -- TODO: inequalities
  -- TODO: concatenation
  -- TODO: slicing
  import Expression.Expression
  import Expression.SOCP
  
  -- helper functions for guards
  isVector :: (Symbol a) => a -> Bool
  isVector x = (rows x) >= 1 && (cols x) == 1

  isScalar :: (Symbol a) => a -> Bool
  isScalar x = (rows x) == 1 && (cols x) == 1

  isMatrix :: (Symbol a) => a -> Bool
  isMatrix x = (rows x) >= 1 && (cols x) >= 1

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


  -- begin list of atoms
  -- in addition to arguments, atoms take a string to uniquely identify/modify their variables
  
  -- square x = x^2
  ecos_square :: Expr -> String -> Expr
  ecos_square x s = expression newVar curvature Positive prog
    where
      curvature = applyDCP Convex monotonicity (vexity x)
      monotonicity = case (sign x) of
        Positive -> Increasing
        Negative -> Decreasing
        otherwise -> Nonmonotone
      prog = (Cones matA vecB kones) <++> (cones x)
      (m,n) = (rows x, cols x)
      newVar = Var ("t"++s) (m, n)
      z0 = Var (vname newVar ++ "z0") (m, n)
      z1 = Var (vname newVar ++ "z1") (m, n)
      matA = [ [(Eye m "0.5", newVar), (Eye m "-1", z0)],
               [(Eye m "-0.5", newVar), (Eye m "-1", z1)] ]
      vecB = [Ones m "-0.5", Ones m "-0.5"]
      kones = [SOCelem [z0, z1, var x]]

  -- quad_over_lin x y = x^Tx / y
  ecos_quad_over_lin :: Expr -> Expr -> String -> Expr
  ecos_quad_over_lin x y s
    | isVector x && isScalar y = expression newVar curvature Positive prog
    | otherwise = none $ "quad_over_lin: " ++ (name y) ++ " is not scalar"
    where
      curvature = applyDCP c1 Decreasing (vexity y)
      c1 = applyDCP Convex monotonicity (vexity x)
      monotonicity = case (sign x) of
        Positive -> Increasing
        Negative -> Decreasing
        otherwise -> Nonmonotone
      prog = (Cones matA vecB kones) <++> (cones x) <++> (cones y)
      newVar = Var ("t"++s) (1, 1)
      z0 = Var (vname newVar ++ "z0") (1, 1)
      z1 = Var (vname newVar ++ "z1") (1, 1)
      matA = [ [(Ones 1 "0.5", var y), (Ones 1 "0.5", newVar), (Ones 1 "-1", z0)],
               [(Ones 1 "0.5", var y), (Ones 1 "-0.5", newVar), (Ones 1 "-1", z1)] ]
      vecB = [Ones 1 "0", Ones 1 "0"]
      kones = [SOC [z0,z1,var x], SOCelem [var y]]

  -- inv_pos(x) = 1/x for x >= 0
  ecos_inv_pos :: Expr -> String -> Expr
  ecos_inv_pos x s = expression newVar curvature Positive prog
    where
      curvature = applyDCP Convex Decreasing (vexity x)
      prog = (Cones matA vecB kones) <++> (cones x)
      (m,n) = (rows x, cols x)
      newVar = Var ("t"++s) (m, n)
      z0 = Var (vname newVar ++ "z0") (m, n)
      z1 = Var (vname newVar ++ "z1") (m, n)
      one = Var (vname newVar ++ "z2") (m, n) -- has to be vector for the SOC code to work (XXX/TODO: allow SOCelem with scalars)
      matA = [ [(Eye m "0.5", var x), (Eye m "0.5", newVar), (Eye m "-1", z0)],
               [(Eye m "0.5", var x), (Eye m "-0.5", newVar), (Eye m "-1", z1)],
               [(Eye m "1", one)] ]
      vecB = [Ones m "0", Ones m "0", Ones m "1"]
      kones = [SOCelem [z0, z1, one], SOCelem [var x]]

  -- constant a = a
  ecos_constant :: Parameter -> String -> Expr
  ecos_constant a s = expression newVar (vexity a) (sign a) (Cones matA vecB [])
    where (m, n) = (rows a, cols a)
          newVar = Var ("t"++s) (m, n)
          matA = [[(Eye m "1", newVar)]]
          vecB = [Vector m (name a)]

  -- mult a x = ax
  ecos_mult :: Parameter -> Expr -> String -> Expr
  ecos_mult a x s
    | isMatrix a && isVector x && compatible = expression newVar curvature sgn prog
    | isScalar a && isVector x = expression newVar curvature sgn prog
    | otherwise = none $ "mult: size of " ++ (name a) ++ " and " ++ (name x) ++ " don't match"
    where
      curvature = applyDCP Affine monotonicity (vexity x)
      monotonicity = case (sign a) of
        Positive -> Increasing
        Negative -> Decreasing
        otherwise -> Nonmonotone
      sgn = (sign a) <*> (sign x)
      compatible = cols a == rows x
      prog = (Cones matA vecB []) <++> (cones x)
      (m,n)
        | isScalar a = (rows x, cols x)
        | otherwise = (rows a, cols x)
      newVar = Var ("t"++s) (m, n)
      matA
        | isScalar a = [ [(Eye m (name a), var x), (Eye m "-1", newVar)] ]
        | otherwise = [ [(Matrix (rows a, cols a) (name a), var x), (Eye m "-1", newVar)] ]
      vecB = [Ones m "0"]

  -- plus x y = x + y
  ecos_plus :: Expr -> Expr -> String -> Expr
  ecos_plus x y s
    | isVector x && isVector y && compatible = expression newVar curvature sgn prog
    | isScalar x && isVector y = expression newVar curvature sgn prog
    | isVector x && isScalar y = expression newVar curvature sgn prog
    | otherwise = none $ "plus: size of " ++ (name x) ++ " and " ++ (name y) ++ " don't match"
    where
      curvature = applyDCP c1 Increasing (vexity y)
      c1 = applyDCP Affine Increasing (vexity x)
      sgn = (sign x) <+> (sign y)
      compatible = cols x == cols y  
      prog = (Cones matA vecB []) <++> (cones x) <++> (cones y)
      (m,n) 
        | isScalar x = (rows y, cols y)
        | otherwise = (rows x, cols x)
      newVar = Var ("t"++s) (m, n)
      matA 
        | isScalar x = [[(Ones m "1", var x), (Eye m "1", var y), (Eye m "-1", newVar)]]
        | isScalar y = [[(Ones m "1", var y), (Eye m "1", var x), (Eye m "-1", newVar)]]
        | otherwise = [ [(Eye m "1", var x), (Eye m "1", var y), (Eye m "-1", newVar)] ]
      vecB = [Ones m "0"]

  -- minus x y = x - y
  ecos_minus :: Expr -> Expr -> String -> Expr
  ecos_minus x y s
    | isVector x && isVector y && compatible = expression newVar curvature sgn prog
    | isScalar x && isVector y = expression newVar curvature sgn prog
    | isVector x && isScalar y = expression newVar curvature sgn prog
    | otherwise = none $ "minus: size of " ++ (name x) ++ " and " ++ (name y) ++ " don't match"
    where
      curvature = applyDCP c1 Decreasing (vexity y)
      c1 = applyDCP Affine Increasing (vexity x)
      sgn = (sign x) <+> neg (sign y)
      compatible = cols x == cols y  
      prog = (Cones matA vecB []) <++> (cones x) <++> (cones y)
      (m,n) 
        | isScalar x = (rows y, cols y)
        | otherwise = (rows x, cols x)
      newVar = Var ("t"++s) (m, n)
      matA 
        | isScalar x = [[(Ones m "1", var x), (Eye m "-1", var y), (Eye m "-1", newVar)]]
        | isScalar y = [[(Ones m "-1", var y), (Eye m "1", var x), (Eye m "-1", newVar)]]
        | otherwise = [ [(Eye m "1", var x), (Eye m "-1", var y), (Eye m "-1", newVar)] ]
      vecB = [Ones m "0"]

  -- neg x = -x
  ecos_negate :: Expr -> String -> Expr
  ecos_negate x s = expression newVar curvature sgn prog
    where
      curvature = applyDCP Affine Decreasing (vexity x)
      sgn = neg (sign x)
      prog = (Cones matA vecB []) <++> (cones x)
      (m,n) = (rows x, cols x)
      newVar = Var ("t"++s) (m, n)
      matA = [ [(Eye m "-1", var x), (Eye m "-1", newVar)] ]
      vecB = [Ones m "0"]

  -- pos(x) = max(x,0)
  ecos_pos :: Expr -> String -> Expr
  ecos_pos x s = expression newVar curvature Positive prog
    where
      curvature = applyDCP Convex Increasing (vexity x)
      prog = (Cones matA vecB kones) <++> (cones x)
      (m,n) = (rows x, cols x)
      newVar = Var ("t"++s) (m, n)
      z0 = Var (vname newVar ++ "z0") (m, n)
      matA = [ [(Eye m "-1", var x), (Eye m "1", newVar), (Eye m "-1", z0)] ]
      vecB = [Ones m "0"]
      kones = [SOCelem [newVar], SOCelem [z0]]

  -- neg(x) = max(-x,0)
  ecos_neg :: Expr -> String -> Expr
  ecos_neg x s = expression newVar curvature Positive prog
    where
      curvature = applyDCP Convex Decreasing (vexity x)
      prog = (Cones matA vecB kones) <++> (cones x)
      (m,n) = (rows x, cols x)
      newVar = Var ("t"++s) (m, n)
      z0 = Var (vname newVar ++ "z0") (m, n)
      matA = [ [(Eye m "1", var x), (Eye m "1", newVar), (Eye m "-1", z0)] ]
      vecB = [Ones m "0"]
      kones = [SOCelem [newVar], SOCelem [z0]]
  
  -- max(x) = max(x_1, x_2, \ldots, x_n)
  ecos_max :: Expr -> String -> Expr
  ecos_max x s = expression newVar curvature (sign x) prog
    where
      curvature = applyDCP Convex Increasing (vexity x)
      prog = (Cones matA vecB kones) <++> (cones x)
      (m, n) = (rows x, cols x)
      newVar = Var ("t"++s) (1, 1)
      z0 = Var (vname newVar ++ "z0") (m, n)
      matA = [[(Ones m "1", newVar), (Eye m "-1", var x), (Eye m "-1", z0)]]
      vecB = [Ones m "0"]
      kones = [SOCelem [z0]]


  -- min(x) = min (x_1, x_2, \ldots, x_n)
  ecos_min :: Expr -> String -> Expr
  ecos_min x s = expression newVar curvature (sign x) prog
    where
      curvature = applyDCP Concave Increasing (vexity x)
      prog = (Cones matA vecB kones) <++> (cones x)
      (m, n) = (rows x, cols x)
      newVar = Var ("t"++s) (1, 1)
      z0 = Var (vname newVar ++ "z0") (m, n)
      matA = [[(Ones m "-1", newVar), (Eye m "1", var x), (Eye m "-1", z0)]]
      vecB = [Ones m "0"]
      kones = [SOCelem [z0]]
 
  -- sum(x) = x_1 + x_2 + ... + x_n
  ecos_sum :: Expr -> String -> Expr
  ecos_sum x s = expression newVar curvature (sign x) prog
    where      
      curvature = applyDCP Affine Increasing (vexity x)
      prog = (Cones matA vecB []) <++> (cones x)
      m = rows x
      newVar = Var ("t"++s) (1, 1)
      matA = [[(Ones 1 "-1", newVar), (OnesT m "1", var x)]]
      vecB = [Ones 1 "0"]

  -- abs(x) = |x|
  ecos_abs :: Expr -> String -> Expr
  ecos_abs x s = expression newVar curvature Positive prog
    where
      curvature = applyDCP Convex monotonicity (vexity x)
      monotonicity = case (sign x) of
        Positive -> Increasing
        Negative -> Decreasing
        otherwise -> Nonmonotone
      prog = (Cones [] [] kones) <++> (cones x)
      (m,n) = (rows x, cols x)
      newVar = Var ("t"++s) (m, n)
      kones = [SOCelem [newVar, var x]]


  -- norm(x) = ||x||_2
  ecos_norm :: Expr -> String -> Expr
  ecos_norm x s = expression newVar curvature Positive prog
    where
      curvature = applyDCP Convex monotonicity (vexity x)
      monotonicity = case (sign x) of
        Positive -> Increasing
        Negative -> Decreasing
        otherwise -> Nonmonotone
      prog = (Cones [] [] kones) <++> (cones x)
      (m,n) = (rows x, cols x)
      newVar = Var ("t"++s) (m, n)
      kones = [SOC [newVar, var x]]

  -- norm_inf(x) = ||x||_\infty
  ecos_norm_inf x s = ecos_max (ecos_abs x (s++"z0")) s

  -- norm1(x) = ||x||_1
  ecos_norm1 x s = ecos_sum (ecos_abs x (s++"z0")) s

  -- sqrt(x) = geo_mean(x,1)
  ecos_sqrt :: Expr -> String -> Expr
  ecos_sqrt x s = expression newVar curvature Positive prog
    where
      curvature = applyDCP Concave Increasing (vexity x)
      prog = (Cones matA vecB kones) <++> (cones x)
      (m,n) = (rows x, cols x)
      newVar = Var ("t"++s) (m,n)
      z0 = Var (vname newVar ++ "z0") (m,n)
      z1 = Var (vname newVar ++ "z1") (m,n)
      matA = [[(Eye m "0.5", var x), (Eye m "-1", z0)],
              [(Eye m "-0.5", var x), (Eye m "-1", z1)]]
      vecB = [Ones m "-0.5", Ones m "-0.5"]
      kones = [SOCelem [z0,z1,newVar]]
      
  -- geo_mean(x,y) = sqrt(x*y)
  ecos_geo_mean :: Expr -> Expr -> String -> Expr
  ecos_geo_mean x y s
    | isScalar x && isScalar y = expression newVar curvature Positive prog
    | otherwise = none $ "geo_mean: " ++ (name x) ++ " and " ++ (name y) ++ " are not scalar"
    where
      curvature = applyDCP c1 Increasing (vexity y)
      c1 = applyDCP Concave Increasing (vexity x)
      prog = (Cones matA vecB kones) <++> (cones x) <++> (cones y)
      newVar = Var ("t"++s) (1,1)
      z0 = Var (vname newVar ++ "z0") (1,1)
      z1 = Var (vname newVar ++ "z1") (1,1)
      matA  =
        [[(Ones 1 "0.5", var x), (Ones 1 "0.5", var y), (Ones 1 "-1", z0)],
        [(Ones 1 "-0.5", var x), (Ones 1 "0.5", var y), (Ones 1 "-1", z1)]]
      vecB = [Ones 1 "0", Ones 1 "0"]
      kones = [SOC [z0, z1, newVar], SOC [var y]]
     
  --   -- pow_rat(x,p,q) <-- not implemented for the moment
  --   -- sum_largest(x,k) <-- also not implemented (uses LP dual)
  --   

  --   
  --   -- special atoms for handling concatenation and (TODO: slicing)
  --   -- (vertical concatenation) [x; y; z; ...]
  --   ecosConcat :: Int->CVXSymbol
  --   ecosConcat nToConcat = Atom {
  --     name="concat",
  --     nargs=nToConcat,
  --     nparams=0,
  --     areValidArgs=(\x -> case (x) of
  --       [] -> True
  --       [x] -> True
  --       x -> let (m,n) = x!!0
  --         in (all (==n) (map snd x))
  --     ),
  --     -- passes in [(n,1), (m,n)], returns (m,1)
  --     symbolSize=(\x -> case(x) of
  --       [] -> (0,0)
  --       [x] -> x
  --       x ->
  --         let (m,n) = x!!0
  --             mtot = foldl (+) 0 (map fst x)
  --         in (mtot, n)
  --     ),
  --     symbolVexity=Affine,
  --     symbolSign=(\x -> if(all (==Positive) x) then Positive
  --       else if (all (==Negative) x) then Negative
  --       else Unknown
  --     ),
  --     monotonicity=(\x -> take nToConcat (repeat Increasing)),
  --     symbolRewrite=(\out inputs ->
  --         let m = rows out
  --             n = cols out
  --             mInputs = map rows inputs
  --             coeffs = zip inputs (map (flip Eye "1") mInputs)
  --         in Problem (Just out) [
  --           (out, Eye m "-1"):coeffs  -- the *first* of this list *must* be the variable to write *out* (otherwise the code will break)
  --         ] [Ones m "0"] []
  --       )
  --   }
