module Atoms.SCOOP_Square(scoop_square) where

  import Expression.Expression
  import Control.Monad.State
  import CodeGenerator.CVX
  import CodeGenerator.Common
  import qualified Data.Map as M


  -- basically, any atom can take anything that can be represented with a var
  -- this includes other Expressions, Vars, etc.
  -- square :: (Vexable a, Signable a) => a -> Expression

  -- so "x" should be a var
  -- square x =
  --   t <- newVar
  --   quad_over_lin x t
  data MyExpr = MyExpr Var Curvature Sign deriving (Show) -- gives expression name, its curvature, and its sign

  type MyExpression = State (Int,SOCP) MyExpr

  -- i could create a vector this way....
  --instance (DCP t) => DCP [t] where
  --  vexity xs = map vexity xs
  --  sign xs = map sign xs

  -- can i write a function that takes a *single* argument and applies it to an arglist?
  -- YES!
  -- ... :: (a -> MyExpression) -> [a] -> MyExpression
  -- it will "concat" across rows of [a] to form "aTranspose", then map f across "aTranspose"

  emptySOCP = SOCP Find (Var "" (1,1)) (ConicSet [] [] [])  -- not sure if this will cause problems....

  -- !#$%&*+./<=>?@\^|-~:
  isIn' :: [Var] -> ([Var] -> SOC) -> State (Int,SOCP) ()
  isIn' xs c = do 
    (count,prob) <- get
    let newCones = constraints prob <++> (ConicSet [] [] [c xs])
        newProb = SOCP (sense prob) (obj prob) newCones
    put (count, newProb)

  -- actually constructs a row
  (.==) :: Row -> Coeff -> State (Int,SOCP) ()
  r .== c = do
    (count, prob) <- get
    let newCones = constraints prob <++> (ConicSet [r] [c] [])
        newProb = SOCP (sense prob) (obj prob) newCones
    put (count, newProb)

  (.+) :: Row -> Row -> Row
  a .+ b = Row (elems a ++ elems b)

  (.*) :: Coeff -> Var -> Row
  a .* b = Row [(a,b)]

  infixl 7 .*
  infixl 6 .+
  infix 4 .==

  minimize :: Var -> State (Int,SOCP) ()
  minimize x = do
    (count, prob) <- get
    let newProb = SOCP Minimize x (constraints prob)
    put (count, newProb)

  maximize :: Var -> State (Int,SOCP) ()
  maximize x = do
    (count, prob) <- get
    let newProb = SOCP Maximize x (constraints prob)
    put (count, newProb)

  find :: Var -> State (Int,SOCP) ()
  find x = do
    (count, prob) <- get
    let newProb = SOCP Find x (constraints prob)
    put (count, newProb)

  subjectTo :: State (Int,SOCP) ()
  subjectTo = return ()  -- nop

  square' :: (Symbol' a) => a -> MyExpression
  square' x = do
    let (m,n) = dimensions' x
    t <- newVar' (m,n)
    z0 <- newVar' (m,n)
    z1 <- newVar' (m,n)

    --positiveSign
    --negativeSign
    --if (all positive) then positiveSign
    --else negativeSign

    --if (any positive) then positiveSign
    --else negativeSign

    -- how to attach signed monotonicity

    -- definition
    minimize t  -- set the objective variable
    subjectTo
    [z0, z1, var' x] `isIn'` SOCelem
    (Eye m 0.5).*t .+ (Eye m (-1)).*z0 .== Ones m (-0.5)
    (Eye m (-0.5)).*t .+ (Eye m (-1)).*z1 .== Ones m (-0.5)

    return $ MyExpr t Convex Positive

  symbolTable' :: M.Map String Expr
  symbolTable' = M.empty

  testagain :: MyExpression
  testagain = do
    t <- square' (3.0 :: Double)  -- this doesn't get "rewritten"... need to fix that somehow
    square' t

  genP = execState testagain (0,emptySOCP)

  testme = cvxgen (Codegen (snd genP) symbolTable')

  class Symbol' a where
    rows' :: a -> Integer
    rows' = rows' . var'
    cols' :: a -> Integer
    cols' = cols' . var'
    dimensions' :: a -> (Integer, Integer)
    dimensions' x = (rows' x, cols' x)
    name' :: a -> String
    name' = name' . var'
    var' :: a -> Var

  instance Symbol' Var where
    rows' = rows
    cols' = cols
    name' = name
    var' x = x 


  instance DCP MyExpr where
    vexity (MyExpr _ c _) = c
    sign (MyExpr _ _ s) = s

  instance DCP Var where
    vexity x = Affine
    sign x = Unknown

  instance DCP Double where
    vexity x = Affine
    sign x
      | x >= 0 = Positive
      | otherwise = Negative

  instance Symbol' Double where
    var' x = Var ("c" ++ display x) (1,1)

  instance Symbol' MyExpr where
    var' (MyExpr v _ _) = v 



  display :: Double -> String
  display = (map (\x -> if (x=='.') then 'd' else x)).show

  -- helper function to construct SOCP for parameters and constants
  parameterSOCP :: Param -> ShapeMod -> SOCP
  parameterSOCP (Param s (m,1)) NoMod = SOCP Find newVar (ConicSet matA vecB [])
    where newVar = Var ("p"++s) (m,1)
          matA = [Row [(Eye m 1, newVar)]]
          vecB = [Vector m (Param s (m,1))]
  parameterSOCP (Param s (1,m)) Transposed = SOCP Find newVar (ConicSet matA vecB [])
    where newVar = Var ("p"++s) (m,1)
          matA = [Row [(Eye m 1, newVar)]]
          vecB = [VectorT m (Param s (1,m))]
  parameterSOCP _ _ = SOCP Find (Var "0" (1,1)) (ConicSet [] [] []) -- matrix parameters fail in to this case

  constantSOCP :: Double -> SOCP
  constantSOCP x = SOCP Find newVar (ConicSet matA vecB [])
    where newVar = Var ("c"++(display x)) (1,1) -- this means if the constant 5 shows up multiple times, it will only create one variable [1;1;1] x = [5;5;5] instead of [1 0 0; 0 1 0; 0 0 1] x = [5;5;5]
          matA = [ Row [(Ones 1 1, newVar)] ]
          vecB = [Ones 1 x]

  -- monadic version
  -- constantSOCP :: Double -> State Int SOCP
  -- constantSOCP x = do
  --  t <- newVar (1,1)
  --  (Ones 1 1) <*> t  <==> (Ones 1 x)
  --  let matA = [ Row [(Ones 1 1, t)] ]
  --      vecB = [Ones 1 x]
  --  return $ SOCP Find t (ConicSet matA vecB [])
          
  -- i guess the state is technically an SOCP that keeps track of the current expression problem def        
  
    --sign (None _) = Unknown
    --sign (Variable _) = Unknown
    --sign (Expr _ _ s _) = s
    --sign (Parameter _ s _) = s
    --sign (Constant x)
    --  | x >= 0 = Positive
    --  | x < 0 = Negative
    --  | otherwise = Unknown

  -- TODO: using monads to simplify the specification of atoms
  -- this allows us to hide a lot of the complexities of DCP
  -- and so on, underneath the hood.
  --
  -- it also makes the code a little harder to re-implement,
  -- but i'll try to keep the comments around to help

  -- newtype MyExpr p d = MyExpr { evalExpr :: (Monotonicity, p) -> (Curvature, Sign, d) } --  p is the argument type of the Atom, d is how an atom is defined

  -- newtype MyExpr s a = MyExpr { runExpr :: s -> (s,a) }
  newtype Property = Property (Curvature, Sign) deriving (Show)

  instance DCP Property where
    vexity (Property (a,_)) = a
    sign (Property (_,b)) = b

  --instance (DCP s) => Monad (MyExpr s) where
  --  return x = MyExpr $ (\s -> (s,x))

  -- the expression state should just be an ExpressionStack.. so it's an RPN stack
  -- operands get pushed on to it. operands are just Expr's

  data ExpressionState = ExpressionState { 
    varcount :: Int,          -- number of new variables introduced so far
    evexity :: Curvature,     -- expression curvature, hmmmmm....
    esign :: Sign,            -- expression sign, hmmmmm.....
    evar :: Var,              -- top-level variable, seeded initially by top-level parser
    econstraints :: ConicSet  -- constraint set, seeded initially by top-level parser
  } deriving (Show)


    -- = Expr Var Curvature Sign ConicSet
    -- | Variable Var
    -- | Parameter Param Sign ShapeMod
    -- | Constant Double
    -- | None String

  --instance Monad (State s) where
  --    return a = State $ \s -> (a, s)
  --    m >>= k  = State $ \s -> let
  --        (a, r) = runState m s
  --        in runState (k a) r

  --get   = State $ \s -> (s, s)
  --put s = State $ \_ -> ((), s)

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

  newVar' :: (Integer, Integer) -> State (Int,SOCP) Var
  newVar' (m,n) = do
    s <- get
    put (fst s+1, snd s)
    return (Var ("t" ++ show (fst s)) (m,n))

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

    --positiveSign
    --negativeSign
    --if (all positive) then positiveSign
    --else negativeSign

    --if (any positive) then positiveSign
    --else negativeSign

    -- how to attach signed monotonicity

    -- if state is just in RPN, then i can pop operands off the stack...
    -- i wanted to do this so minimize could just pop things off the stack
    -- to check vexity

    -- minimize t -- minimize, maximize, or find give the "vexity" of the atom
    -- subjectTo -- this can be a nop
    --    [z0, z1, var x] `isIn` SOCelem
    --    (Eye m 0.5) <*> t <+> (Eye m (-1)) <*> z0 <==> Ones m (-0.5)
    --    (Eye m (-0.5)) <*> t <+> (Eye m (-1)) <*> z1 <==> Ones m (-0.5)


    -- minimize t s.t. 
    --   0.5|*|t |-| z0 `eq` (-0.5)
    --   (-0.5)|*|t |-| z1 `eq` (-0.5)
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


  -- t <- newVar (3,1)
  -- a <- newParam (5,3)
  --
  -- a*t == 0

  -- with RPN state, things will look more like...
  -- do
  --  scoop_square
  --  scoop_plus
  --  scoop_minus
  -- etc.
  -- not quite what i'd like...

  -- i'd like something like
  -- y <- scoop_square [x]
  -- y <- scoop_plus [x,y]
  -- scoop_minus [x,y]
  -- etc.

  -- this makes it impossible to write things like
  -- scoop_square [scoop_plus [x,y]]