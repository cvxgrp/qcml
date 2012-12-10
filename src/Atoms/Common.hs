module Atoms.Common(ShapedVar(..), isIn, Paramed, Expression, ExpressionState, newVar, module Expression.Expression, 
  subjectTo, find, minimize, maximize, (.*), (.+), (.==), MyExpr(..), emptySOCP, initState, (<&>), positive, negative, unknown, 
  increasing, decreasing, nonmonotone) where


  -- the goal of using "monads" to encapsulate my atom definitions is to
  -- make the atom definitions easy to read and understand while hiding all
  -- the heavy lifting that happens under the hood


  import Expression.Expression
  import Control.Monad.State
  -- "and" and "find" are atom keywords, so we exclude them


  -- basically, any atom can take anything that can be represented with a var
  -- this includes other Expressions, Vars, etc.
  -- square :: (Vexable a, Signable a) => a -> Expression

  -- so "x" should be a var
  -- square x =
  --   t <- newVar
  --   quad_over_lin x t

  -- the expression state should just be an ExpressionStack.. so it's an RPN stack
  -- operands get pushed on to it. operands are just Expr's

  --data ExpressionState = ExpressionState { 
  --  count' :: Int,          -- number of new variables introduced so far
  --  vexity' :: Curvature,     -- expression curvature, hmmmmm....
  --  sign' :: Sign,            -- expression sign, hmmmmm.....
  --  socp' :: SOCP             -- top-level variable, seeded initially by top-level parser
  --} deriving (Show)

  type ExpressionState = (Integer, Curvature, Sign, SOCP)

  initState = (0, Affine, Unknown, emptySOCP)

  -- this thing is really for parser's use
  -- curvature and sign will belong in state now....
  data MyExpr = MyExpr Var Curvature Sign -- gives expression name, its curvature, and its sign
              | MyParam Var Param Sign -- gives param's name (its var) and its value (a param)
              | MyConstant Var Double -- gives constant's name (its var) and its value (a double)

  type Expression = State ExpressionState MyExpr

  -- i could create a vector this way....
  --instance (DCP t) => DCP [t] where
  --  vexity xs = map vexity xs
  --  sign xs = map sign xs

  -- can i write a function that takes a *single* argument and applies it to an arglist?
  -- YES!
  -- ... :: (a -> Expression) -> [a] -> Expression
  -- it will "concat" across rows of [a] to form "aTranspose", then map f across "aTranspose"

  emptySOCP = SOCP Find (Var "" (1,1)) (ConicSet [] [] [])  -- not sure if this will cause problems....

  -- !#$%&*+./<=>?@\^|-~:
  isIn :: [Var] -> ([Var] -> SOC) -> State ExpressionState ()
  isIn xs c = do 
    (count, vexity, sign, prob) <- get
    let newCones = constraints prob <++> (ConicSet [] [] [c xs])
        newProb = SOCP (sense prob) (obj prob) newCones
    put (count, vexity, sign, newProb)

  -- actually constructs a row
  (.==) :: Row -> Coeff -> State ExpressionState ()
  r .== c = do
    (count, vexity, sign, prob) <- get
    let newCones = constraints prob <++> (ConicSet [r] [c] [])
        newProb = SOCP (sense prob) (obj prob) newCones
    put (count, vexity, sign, newProb)

  (.+) :: Row -> Row -> Row
  a .+ b = Row (elems a ++ elems b)

  (.*) :: Coeff -> Var -> Row
  a .* b = Row [(a,b)]

  infixl 7 .*
  infixl 6 .+
  infix 4 .==

  minimize :: Var -> State ExpressionState (Curvature, Sign)
  minimize x = do
    (count, vexity, sign, prob) <- get
    case (vexity) of
      Nonconvex -> fail "cannot minimize nonconvex expression" -- XXX: error message may need to change
      Concave -> fail "cannot minimize concave expression"
      otherwise -> do
        let newProb = SOCP Minimize x (constraints prob)
        put (count, Convex, sign, newProb)
        return (Convex, sign)

  maximize :: Var -> State ExpressionState (Curvature, Sign)
  maximize x = do
    (count, vexity, sign, prob) <- get
    case (vexity) of
      Nonconvex -> fail "cannot maximize nonconvex expression" -- XXX: error message may need to change
      Convex -> fail "cannot maximize convex expression"
      otherwise -> do
        let newProb = SOCP Maximize x (constraints prob)
        put (count, Concave, sign, newProb)
        return (Concave, sign)

  find :: Var -> State ExpressionState (Curvature, Sign)
  find x = do
    (count, vexity, sign, prob) <- get
    case (vexity) of
      Nonconvex -> fail "cannot find over nonconvex expression" -- XXX: error message may need to change
      otherwise -> do
        let newProb = SOCP (sense prob) x (constraints prob)
        put (count, vexity, sign, newProb)
        return (vexity, sign)

  subjectTo :: State ExpressionState ()
  subjectTo = return ()  -- nop


  positive :: State ExpressionState ()
  positive = do
    (count, vexity, sign, prob) <- get
    put (count, vexity, Positive, prob)

  negative :: State ExpressionState ()
  negative = do
    (count, vexity, sign, prob) <- get
    put (count, vexity, Negative, prob)

  -- do i need this?
  unknown :: State ExpressionState ()
  unknown = do
    (count, vexity, sign, prob) <- get
    put (count, vexity, Unknown, prob)

  increasing :: (ShapedVar a) => a -> State ExpressionState ()
  increasing x = do
    (count, vexity, sign, prob) <- get
    case(vexity'' x) of
      Affine -> put (count, Affine, sign, prob)
      Convex -> put (count, Convex, sign, prob)
      Concave -> put (count, Concave, sign, prob)
      otherwise -> put (count, Nonconvex, sign, prob)

  decreasing :: (ShapedVar a) => a -> State ExpressionState ()
  decreasing x = do
    (count, vexity, sign, prob) <- get
    case(vexity'' x) of
      Affine -> put (count, Affine, sign, prob)
      Convex -> put (count, Concave, sign, prob)
      Concave -> put (count, Convex, sign, prob)
      otherwise -> put (count, Nonconvex, sign, prob)

  nonmonotone :: (ShapedVar a) => a -> State ExpressionState ()
  nonmonotone x = do
    (count, vexity, sign, prob) <- get
    case(vexity'' x) of
      Affine -> put (count, Affine, sign, prob)
      otherwise -> put (count, Nonconvex, sign, prob)

  -- just an alias to sequence together ops
  (<&>) :: (Monad m) => m a -> m b -> m b
  x <&> y = x >> y


  newVar :: Integer -> State ExpressionState Var
  newVar m = do
    (count, vexity, sign, prob) <- get
    put (count+1, vexity, sign, prob)
    return (Var ("t" ++ show count) (m,1))
 

  -- stuff belongs in Expression.Expression
  class Paramed a where


  class ShapedVar a where
    --isParam :: a -> Bool
    --isParam _ = False
    --isConst :: a -> Bool
    --isConst _ = False
    rows'' :: a -> Integer
    rows'' = rows'' . var''
    cols'' :: a -> Integer
    cols'' = cols'' . var''
    dimensions'' :: a -> (Integer, Integer)
    dimensions'' x = (rows'' x, cols'' x)
    sign'' :: a -> Sign
    vexity'' :: a -> Curvature
    var'' :: a -> Var

  instance ShapedVar Var where
    rows'' = rows
    cols'' = cols
    var'' x = x
    sign'' x = Unknown
    vexity'' x = Affine

  instance ShapedVar MyExpr where
    var'' (MyExpr v _ _) = v
    var'' (MyConstant v _) = v 
    var'' (MyParam v _ _) = v 

    sign'' (MyExpr _ _ s) = s
    sign'' (MyConstant _ d)
      | d >= 0 = Positive
      | otherwise = Negative
    sign'' (MyParam _ _ s) = s

    vexity'' (MyExpr _ c _) = c
    vexity'' (MyConstant _ _) = Affine
    vexity'' (MyParam _ _ _) = Affine

    --isParam (MyParam _ _ _) = True
    --isParam _ = False

    --isConst (MyConstant _ _) = True
    --isConst _ = False

  instance Paramed Param where

  instance Paramed Double where