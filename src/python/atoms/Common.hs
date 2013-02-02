module Atoms.Common(Rewriter, newVar, addLine, getString, initState, Rewriteable(..),
  increasing, decreasing, nonmonotone, (<&>), RewriterState) where


  -- the goal of using "monads" to encapsulate my atom definitions is to
  -- make the atom definitions easy to read and understand while hiding all
  -- the heavy lifting that happens under the hood


  import Expression.Expression
  import Control.Monad.State
  import Data.List (sortBy)

  type RewriterState = (Integer, [String])

  initState = (0, [])

  -- An "Rewriter" is a monad that carries a state (RewriterState) and contains
  -- an Expr object. A "Statement" contains nothing upon "return".

  -- a "Rewriter" fails when DCP rule is violated

  --type Rewriter = State RewriterState Expr
  --type Statement = State RewriterState ()
  type Rewriter a = State RewriterState a

  newVar :: Rewriter String
  newVar = do
    (c, prob) <- get
    let v = "t" ++ show c
    put (c+1, prob ++ ["_variable " ++ v])
    return v

  addLine :: String -> Rewriter ()
  addLine s = do
    (c,prob) <- get
    put (c, prob ++ [s])

  -- gets the string (problem) associated with the state
  getString :: RewriterState -> String
  getString (_,x) = unlines x

  -- allows Expr, Params, Doubles to be turned in to Rewriters
  class Rewriteable a where
    express :: a -> Rewriter Expr
    parameterize :: a -> Rewriter Param

  instance Rewriteable Expr where
    express x = return x
    parameterize = fail "cannot turn an expression into a paramteter"
  
  instance Rewriteable Param where
    express (Param x s) = do
      t <- newVar
      addLine $ concat [t, " == ", x]
      return (Expr t Affine s)
    -- TODO: express _ = fail "cannot create matrix variable for matrix parameter"

    parameterize x = return x

  instance Rewriteable Double where
    express x = do
      t <- newVar
      addLine $ concat [t, " == ", show x]
      let s | x >= 0 = Positive
            | otherwise = Negative
      return (Expr t Affine s)

    parameterize x = return $ Param (show x) sign
      where sign | x >= 0 = Positive
                 | x < 0 = Negative

  instance Rewriteable Symbol where
    express (ESym e) = express e
    express (PSym p) = express p
    express (CSym c) = express c

    parameterize (ESym e) = parameterize e
    parameterize (PSym p) = parameterize p
    parameterize (CSym c) = parameterize c



  -- Double to Param
  -- also have Param to Rewriter
  -- and have Double to Rewriter directly

  -- generic function that checks argument in arglist
  -- all arguments must have same rows *or* some must be scalar
  --
  --   although dimensions are abstract, assumes that differently named dimensions
  --   are going to have different numeric values

  --isValidArgs :: Symbolic a => [a] -> Bool
  --isValidArgs exprs = all checkFun (tail sortedExprs)
  --  where sortedExprs = sortBy rowOrdering exprs -- have to sort in case scalars are first argument
  --        checkFun = (\x -> x==m || x=="1").rows
  --        m = rows (head sortedExprs)

  --rowOrdering :: Symbolic a => a -> a -> Ordering
  --rowOrdering x y
  --  | rows x < rows y = GT
  --  | rows x > rows y = LT
  --  | otherwise = EQ



  -- basically, any atom can take anything that can be represented with a var
  -- this includes other Rewriters, Vars, etc.
  -- square :: (Vexable a, Signable a) => a -> Rewriter

  -- so "x" should be a var
  -- square x =
  --   t <- newVar
  --   quad_over_lin x t

  -- the Rewriter state should just be an RewriterStack.. so it's an RPN stack
  -- operands get pushed on to it. operands are just Expr's

  --data RewriterState = RewriterState { 
  --  count' :: Int,          -- number of new variables introduced so far
  --  vexity' :: Curvature,     -- Rewriter curvature, hmmmmm....
  --  sign' :: Sign,            -- Rewriter sign, hmmmmm.....
  --  socp' :: SOCP             -- top-level variable, seeded initially by top-level parser
  --} deriving (Show)

  ---- new problem state for lang-to-lang xform
  ---- type ProblemState = (Integer, giString), the String is the current program (as a string)
  --type RewriterState = (Integer, Curvature, Sign, SOCP)

  --initState = (0, Affine, Unknown, emptySOCP)

  ---- this thing is really for parser's use
  ---- curvature and sign will belong in state now....
  --data MyExpr = MyExpr Var Curvature Sign -- gives Rewriter name, its curvature, and its sign
  --            | MyParam Var Param Sign -- gives param's name (its var) and its value (a param)
  --            | MyConstant Var Double -- gives constant's name (its var) and its value (a double)

  --type Rewriter = State RewriterState MyExpr

  ---- i could create a vector this way....
  ----instance (DCP t) => DCP [t] where
  ----  vexity xs = map vexity xs
  ----  sign xs = map sign xs

  ---- can i write a function that takes a *single* argument and applies it to an arglist?
  ---- YES!
  ---- ... :: (a -> Rewriter) -> [a] -> Rewriter
  ---- it will "concat" across rows of [a] to form "aTranspose", then map f across "aTranspose"

  --emptySOCP = SOCP Find (Var "" (1,1)) (ConicSet [] [] [])  -- not sure if this will cause problems....

  ---- !#$%&*+./<=>?@\^|-~:
  --isIn :: [Var] -> ([Var] -> SOC) -> State RewriterState ()
  --isIn xs c = do 
  --  (count, vexity, sign, prob) <- get
  --  let newCones = constraints prob <++> (ConicSet [] [] [c xs])
  --      newProb = SOCP (sense prob) (obj prob) newCones
  --  put (count, vexity, sign, newProb)

  ---- actually constructs a row
  --(.==) :: Row -> Coeff -> State RewriterState ()
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

  ---- a convex function of an Rewriter
  --convex :: (Curvature, Sign)
  --convex = (Convex, )

  ---- a concave function of an Rewriter
  --concave :: (Curvature, Sign)
  --concave = (Concave, )

  ---- an affine function of an Rewriter
  --affine :: (Curvature, Sign)
  --affine = (Affine, )

  ---- sign ops set curvature to Affine (why? so that chaining increasing and decreasing will do the right thing)
  ---- now, order matters...
  ----
  ---- positive; increasing x; increasing y
  ---- different from
  ---- increasing x; positive; increasing y
  
  --positive :: Rewriter
  --positive = do
  --  (count, vexity, sign, prob) <- get
  --  put (count, Affine, Positive, prob)

  --negative :: Rewriter
  --negative = do
  --  (count, vexity, sign, prob) <- get
  --  put (count, Affine, Negative, prob)

  ---- do i need this?
  --unknown :: Rewriter
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


  --newVar :: Integer -> State RewriterState Var
  --newVar m = do
  --  (count, vexity, sign, prob) <- get
  --  put (count+1, vexity, sign, prob)
  --  return (Var ("t" ++ show count) (m,1))
 

  ---- stuff belongs in Rewriter.Rewriter
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