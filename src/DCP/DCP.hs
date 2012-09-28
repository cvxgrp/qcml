module DCP.DCP (Monotonicity(..), 
  Vexity(..), 
  Sign(..)) where
    
  -- TODO: eliminate distinction between 1 and 2 argument operations

  -- quad over lin x y = minimize t subject to ((y+t)/2, (y-t)/2, x) in SOC3, y in SOC1
  
  -- data types for signed monotonicity DCP rules
  data Monotonicity = Increasing 
    | Decreasing 
    | Nonmonotone 
    deriving (Show, Eq)
  data Vexity = Convex 
    | Concave 
    | Affine 
    | Nonconvex 
    deriving (Show, Eq)
  data Sign = Positive
    | Negative
    | Unknown
    deriving (Show, Eq)
  
  -- Type classes
  
  -- All symbols have a vexity and a sign
  class Symbol a where
    vexity :: a -> Vexity
    sign :: a -> Sign
  
  -- All atoms are symbols which have monotonicity and 
  -- can be applied
  class Symbol a => Atom a where
    monotonicity :: a -> [Sign] -> [Monotonicity]  
    applySign :: a -> [Sign] -> Sign
    applyDCP :: a -> [Sign] -> [Vexity] -> Vexity
    applyDCP f s v = 
      let m = monotonicity f s
      in foldl reduceVex (vexity f) (zipWith inferVexity v m)
    -- rewrite
  
  inferVexity :: Vexity -> Monotonicity -> Vexity
  inferVexity Convex Increasing = Convex
  inferVexity Convex Decreasing = Concave
  inferVexity Concave Decreasing = Convex
  inferVexity Concave Increasing = Concave
  inferVexity Affine _ = Affine
  inferVexity _ _ = Nonconvex
  
  reduceVex :: Vexity -> Vexity -> Vexity
  reduceVex Nonconvex _ = Nonconvex
  reduceVex _ Nonconvex = Nonconvex
  reduceVex Affine s = s
  reduceVex s Affine = s
  reduceVex Convex Convex = Convex
  reduceVex Concave Concave = Concave
  reduceVex _ _ = Nonconvex

  -- it turns out that Parameter, Variables, and Functions are symbols
  -- but Expressions are *also* symbols, i.e., [Symbol] is also a Symbol
  
    
      
    -- rewrite
  
  --
    -- 
    -- data Expr a = Expr String
    -- 
    -- -- Symbols, the ops will carry their definitions
    -- data CVXSymbol = BinaryOp String
    --   | UnaryOp String
    --   | Variable
    --   | Parameter
    --   deriving (Show)
    -- 
    -- -- An expression is made up of symbols
    -- data Expression a = Nil
    --   | Leaf a
    --   | Node a [Expression a]
    --   deriving (Show)
    --   
    -- type CVXExpression = Expression CVXSymbol
    -- type RPNStack = [CVXSymbol]
    -- 
    -- -- helper functions
    -- isConvex :: Vexity a => a -> Bool
    -- isConvex x = not (vexity x == Concave)
    -- 
    -- isConcave :: Vexity a => a -> Bool
    -- isConcave x = not (vexity x == Convex)
    -- 
    -- isAffine :: Vexity a => a -> Bool
    -- isAffine x = isConvex x && isConcave x
    -- 
    -- isPositive :: Sign a => a -> Bool
    -- isPositive x = (sign x) == Positive
    -- 
    -- isNegative :: Sign a => a -> Bool
    -- isNegative x = not (isPositive x)
    -- 
    -- 
    -- -- converts RPN to an expression tree, if you ever needed it...
    -- testme :: RPNStack -> [CVXExpression] -> CVXExpression
    -- testme [] [x] = x
    -- testme [] _ = Nil
    -- testme (x:xs) (e1:e2:expr) = case (x) of
    --   BinaryOp a -> testme xs ((Node (BinaryOp a) [e1,e2]):expr)
    --   UnaryOp a -> testme xs ((Node (UnaryOp a) [e1]):e2:expr)
    --   otherwise -> testme xs ((Leaf x):e1:e2:expr)
    -- testme (x:xs) (e1:expr) = case (x) of
    --   UnaryOp a -> testme xs ((Node (UnaryOp a) [e1]):expr)
    --   otherwise -> testme xs ((Leaf x):e1:expr)
    -- testme (x:xs) expr = testme xs ((Leaf x):expr)
    -- 
    -- 

  
  -- i'd like to be able to write
  -- instance Atom ECOQuadOverLin where
  --   vexity = Convex
  --   monotonicity [s1,s2]
  --    | s1==Positive -> [Increasing, Decreasing]
  --    | otherwise -> [Nonmonotone, Decreasing]
  --   sign _ = Positive
  
