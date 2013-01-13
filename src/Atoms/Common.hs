module Atoms.Common(Expression, newVar, addLine, getString, initState, Expressive(..),
  isValidArgs, primitiveAdd, primitiveMinus, scoop_eq, scoop_leq, scoop_geq) where


  -- the goal of using "monads" to encapsulate my atom definitions is to
  -- make the atom definitions easy to read and understand while hiding all
  -- the heavy lifting that happens under the hood


  import Expression.Expression
  import Control.Monad.State
  import Data.List (sortBy)

  type ExpressionState = (Integer, [String])

  initState = (0, [])

  -- An "Expression" is a monad that carries a state (ExpressionState) and contains
  -- an Expr object. A "Statement" contains nothing upon "return".
  --type Expression = State ExpressionState Expr
  --type Statement = State ExpressionState ()
  type Expression = State ExpressionState

  newVar :: String -> Expression String
  newVar m = do
    (c, prob) <- get
    let v = "t" ++ show c
    put (c+1, prob ++ ["variable " ++ v ++ "(" ++ m ++ ")"])
    return v

  addLine :: String -> Expression ()
  addLine s = do
    (c,prob) <- get
    put (c, prob ++ [s])

  -- gets the string (problem) associated with the state
  getString :: ExpressionState -> String
  getString (_,x) = unlines x

  -- allows Expr, Params, Doubles to be turned in to Expressions
  class Expressive a where
    express :: a -> Expression Expr

  instance Expressive Expr where
    express x = return x
  
  instance Expressive Param where
    express (Param x r "1" s) = do
      t <- newVar r
      addLine $ concat [t, " == ", x]
      return (Expr t r Affine s)
    express _ = fail "cannot create matrix variable for matrix parameter"

  instance Expressive Double where
    express x = do
      t <- newVar "1"
      addLine $ concat [t, " == ", show x]
      let s | x >= 0 = Positive
            | otherwise = Negative
      return (Expr t "1" Affine s)

  instance Expressive Symbol where
    express (ESym x) = express x
    express (PSym x) = express x
    express (CSym x) = express x
  

  -- generic function that checks argument in arglist
  -- all arguments must have same rows *or* some must be scalar
  --
  --   although dimensions are abstract, assumes that differently named dimensions
  --   are going to have different numeric values
  isValidArgs :: Symbolic a => [a] -> Bool
  isValidArgs exprs = all checkFun (tail sortedExprs)
    where sortedExprs = sortBy rowOrdering exprs -- have to sort in case scalars are first argument
          checkFun = (\x -> x==m || x=="1").rows
          m = rows (head sortedExprs)

  rowOrdering :: Symbolic a => a -> a -> Ordering
  rowOrdering x y
    | rows x < rows y = GT
    | rows x > rows y = LT
    | otherwise = EQ


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

  ---- new problem state for lang-to-lang xform
  ---- type ProblemState = (Integer, giString), the String is the current program (as a string)
  --type ExpressionState = (Integer, Curvature, Sign, SOCP)

  --initState = (0, Affine, Unknown, emptySOCP)

  ---- this thing is really for parser's use
  ---- curvature and sign will belong in state now....
  --data MyExpr = MyExpr Var Curvature Sign -- gives expression name, its curvature, and its sign
  --            | MyParam Var Param Sign -- gives param's name (its var) and its value (a param)
  --            | MyConstant Var Double -- gives constant's name (its var) and its value (a double)

  --type Expression = State ExpressionState MyExpr

  ---- i could create a vector this way....
  ----instance (DCP t) => DCP [t] where
  ----  vexity xs = map vexity xs
  ----  sign xs = map sign xs

  ---- can i write a function that takes a *single* argument and applies it to an arglist?
  ---- YES!
  ---- ... :: (a -> Expression) -> [a] -> Expression
  ---- it will "concat" across rows of [a] to form "aTranspose", then map f across "aTranspose"

  --emptySOCP = SOCP Find (Var "" (1,1)) (ConicSet [] [] [])  -- not sure if this will cause problems....

  ---- !#$%&*+./<=>?@\^|-~:
  --isIn :: [Var] -> ([Var] -> SOC) -> State ExpressionState ()
  --isIn xs c = do 
  --  (count, vexity, sign, prob) <- get
  --  let newCones = constraints prob <++> (ConicSet [] [] [c xs])
  --      newProb = SOCP (sense prob) (obj prob) newCones
  --  put (count, vexity, sign, newProb)

  ---- actually constructs a row
  --(.==) :: Row -> Coeff -> State ExpressionState ()
  --r .== c = do
  --  (count, vexity, sign, prob) <- get
  --  let newCones = constraints prob <++> (ConicSet [r] [c] [])
  --      newProb = SOCP (sense prob) (obj prob) newCones
  --  put (count, vexity, sign, newProb)

  --(.+) :: Row -> Row -> Row
  --a .+ b = Row (elems a ++ elems b)

  --(.*) :: Coeff -> Var -> Row
  --a .* b = Row [(a,b)]

  --infixl 7 .*
  --infixl 6 .+
  --infix 4 .==

  --  t = increasing x
  --  r = increasing y
  --  r = convex (convex t)

  --  convex [(increasing x) (increasing y)]

  ---- a convex function of an expression
  --convex :: (Curvature, Sign)
  --convex = (Convex, )

  ---- a concave function of an expression
  --concave :: (Curvature, Sign)
  --concave = (Concave, )

  ---- an affine function of an expression
  --affine :: (Curvature, Sign)
  --affine = (Affine, )

  ---- sign ops set curvature to Affine (why? so that chaining increasing and decreasing will do the right thing)
  ---- now, order matters...
  ----
  ---- positive; increasing x; increasing y
  ---- different from
  ---- increasing x; positive; increasing y
  
  --positive :: Expression
  --positive = do
  --  (count, vexity, sign, prob) <- get
  --  put (count, Affine, Positive, prob)

  --negative :: Expression
  --negative = do
  --  (count, vexity, sign, prob) <- get
  --  put (count, Affine, Negative, prob)

  ---- do i need this?
  --unknown :: Expression
  --unknown = do
  --  (count, vexity, sign, prob) <- get
  --  put (count, Affine, Unknown, prob)

  increasing :: Expr -> Curvature
  increasing x = case(vexity x) of
      Affine -> Affine
      Convex -> Convex
      Concave -> Concave
      otherwise -> Nonconvex

  decreasing :: Expr -> Curvature
  decreasing x = case(vexity x) of
      Affine -> Affine
      Convex -> Concave
      Concave -> Convex
      otherwise -> Nonconvex
  
  nonmonotone :: Expr -> Curvature
  nonmonotone x = case(vexity x) of
    Affine -> Affine
    otherwise -> Nonconvex

  -- just an alias to sequence together ops
  (<&>) :: Curvature -> Curvature -> Curvature
  Affine <&> y = y
  x <&> Affine = x
  Convex <&> Convex = Convex
  Concave <&> Concave = Concave
  _ <&> _ = Nonconvex

  -- TODO: divide in to primitives (plus, minus, multiply), scalar atoms, and vector atoms


  --propVexity :: Curvature -> Curvature -> Curvature
  --propVexity Nonconvex _ = Nonconvex
  --propVexity _ Nonconvex = Nonconvex
  --propVexity x Affine = x
  --propVexity Affine x = x
  --propVexity Convex Convex = Convex
  --propVexity Concave Concave = Concave
  --propVexity _ _ = Nonconvex


  --newVar :: Integer -> State ExpressionState Var
  --newVar m = do
  --  (count, vexity, sign, prob) <- get
  --  put (count+1, vexity, sign, prob)
  --  return (Var ("t" ++ show count) (m,1))
 

  ---- stuff belongs in Expression.Expression
  --class Paramed a where


  --class ShapedVar a where
  --  --isParam :: a -> Bool
  --  --isParam _ = False
  --  --isConst :: a -> Bool
  --  --isConst _ = False
  --  rows'' :: a -> Integer
  --  rows'' = rows'' . var''
  --  cols'' :: a -> Integer
  --  cols'' = cols'' . var''
  --  dimensions'' :: a -> (Integer, Integer)
  --  dimensions'' x = (rows'' x, cols'' x)
  --  sign'' :: a -> Sign
  --  vexity'' :: a -> Curvature
  --  var'' :: a -> Var

  --instance ShapedVar Var where
  --  rows'' = rows
  --  cols'' = cols
  --  var'' x = x
  --  sign'' x = Unknown
  --  vexity'' x = Affine

  --instance ShapedVar MyExpr where
  --  var'' (MyExpr v _ _) = v
  --  var'' (MyConstant v _) = v 
  --  var'' (MyParam v _ _) = v 

  --  sign'' (MyExpr _ _ s) = s
  --  sign'' (MyConstant _ d)
  --    | d >= 0 = Positive
  --    | otherwise = Negative
  --  sign'' (MyParam _ _ s) = s

  --  vexity'' (MyExpr _ c _) = c
  --  vexity'' (MyConstant _ _) = Affine
  --  vexity'' (MyParam _ _ _) = Affine

  --  --isParam (MyParam _ _ _) = True
  --  --isParam _ = False

  --  --isConst (MyConstant _ _) = True
  --  --isConst _ = False

  --instance Paramed Param where

  --instance Paramed Double where