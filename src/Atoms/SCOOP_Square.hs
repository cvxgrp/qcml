module Atoms.SCOOP_Square(scoop_square) where

  import Expression.Expression
  import Control.Monad.State


  type Expression = State Int Expr

  -- state should be Int, Current ConicSet, and top-level Var

  --instance Monad MyVexity where
  --  return = 

  --class Monad m where  
  --  return :: a -> m a  
  
  --  (>>=) :: m a -> (a -> m b) -> m b  
  
  --  (>>) :: m a -> m b -> m b  
  --  x >> y = x >>= \_ -> y  
  
  --  fail :: String -> m a  
  --  fail msg = error msg  

  -- TODO: insert some explanation about monads in all atom definitions
  newVar :: (Integer, Integer) -> State Int Var
  newVar (m,n) = do
    s <- get
    put (s+1)
    return (Var ("t" ++ show s) (m,n))

  -- !#$%&*+./<=>?@\^|-~:
  isIn :: [Var] -> ([Var] -> SOC) -> ConicSet
  isIn xs c = ConicSet [] [] [c xs]

  (<==>) :: Row -> Coeff -> ConicSet
  r <==> c = ConicSet [r] [c] []

  (<+>) :: Row -> Row -> Row
  a <+> b = Row (elems a ++ elems b)

  (<*>) :: Coeff -> Var -> Row
  a <*> b = Row [(a,b)]

  infixl 7 <*>
  infixl 6 <+>
  infix 4 <==>

  scoop_square_over_lin :: Expression -> Expression -> Expression
  scoop_square_over_lin x y = do
    t <- newVar (3,1)
    return $ Variable t

  scoop_square :: [Expression] -> Expression
  scoop_square [y]  = do
    x <- y
    let (m,n) = (rows x, cols x)
    t <- newVar (m,n)
    z0 <- newVar (m,n)
    z1 <- newVar (m,n)
    -- minimize t s.t. 
    --   (Eye m 0.5)|*|t |+| (Eye m -1)|*|z0 `eq` Ones m (-0.5)
    --   (Eye m (-0.5))|*|t |+| (Eye m -1)|*|z1 `eq` Ones m (-0.5)
    --   [z0,z1,var x] `isIn` SOCelem
    let 
      curvature = applyDCP Convex monotonicity (vexity x)
      monotonicity = case (sign x) of
        Positive -> Increasing
        Negative -> Decreasing
        otherwise -> Nonmonotone
      prog = ([z0, z1, var x] `isIn` SOCelem) <++> 
             ((Eye m 0.5) <*> t <+> (Eye m (-1)) <*> z0 <==> Ones m (-0.5)) <++> 
             ((Eye m (-0.5)) <*> t <+> (Eye m (-1)) <*> z1 <==> Ones m (-0.5)) <++>
             (cones x)
      --matA = [ Row [(Eye m 0.5, t), (Eye m (-1), z0)],
      --         Row [(Eye m (-0.5), t), (Eye m (-1), z1)] ]
      --vecB = [Ones m (-0.5), Ones m (-0.5)]
      --kones = [ [z0, z1, var x] `isIn` SOCelem ]
    case x of
      None _ -> y
      otherwise -> return $ expression t curvature Positive prog

  scoop_square _ = return $ None "scoop_square: too many arguments"

  testcase :: Expression
  testcase = do {
    scoop_square $ [scoop_square [x]];
  } where x = return $ variable "x" (4,2)

  funny :: State Int (Expr,Int)
  funny = return (runState testcase 0)

  test = runState funny 0
  -- -- square x = x^2
  --scoop_square :: Expr -> String -> Expr
  --scoop_square (None s) _ = None s
  --scoop_square x s = expression newVar curvature Positive prog
  --  where
  --    curvature = applyDCP Convex monotonicity (vexity x)
  --    monotonicity = case (sign x) of
  --      Positive -> Increasing
  --      Negative -> Decreasing
  --      otherwise -> Nonmonotone
  --    prog = (ConicSet matA vecB kones) <++> (cones x)
  --    (m,n) = (rows x, cols x)
  --    newVar = Var ("t"++s) (m, n)
  --    z0 = Var (name newVar ++ "z0") (m, n)
  --    z1 = Var (name newVar ++ "z1") (m, n)
  --    matA = [ Row [(Eye m 0.5, newVar), (Eye m (-1), z0)],
  --             Row [(Eye m (-0.5), newVar), (Eye m (-1), z1)] ]
  --    vecB = [Ones m (-0.5), Ones m (-0.5)]
  --    kones = [SOCelem [z0, z1, var x]]