module DCP.DCP (Monotonicity(..), 
  Vexity(..), 
  Sign(..),
  Symbol(..),
  Atom(..)) where
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
    -- data Function = Function deriving (Show)
    -- 
    -- functd :: Vexity a => a -> Function
    -- functd a = Function
    -- 
    -- data ECOSPlus = ECOSPlus
    -- data ECOSMinus = ECOSMinus
    -- 
    -- instance Vexity ECOSPlus where
    --   vexity _ = Affine
    -- 
    -- instance Vexity ECOSMinus where
    --   vexity _ = Affine
    -- 
    -- testme1 :: [Function]
    -- testme1 = [functd ECOSPlus, functd ECOSMinus]
    -- 
    -- 
    -- class (Vexity a, Sign a, Monotonicity a) => Atom a where
    --   
    -- data ECOSQuadOverLin = ECOSQuadOverLin
    -- 
    -- instance Vexity ECOSQuadOverLin where
    --   vexity _ = Convex
    -- instance Monotonicity ECOSQuadOverLin where
    --   monotonicity _ [s1, s2]
    --     | s1 == Positive = [Increasing, Decreasing]
    --     | otherwise = [Nonmonotone, Decreasing]
    -- instance Sign ECOSQuadOverLin where
    --   sign _ = Positive
  --instance Atom ECOSQuadOverLin where

  
  -- i'd like to be able to write
  -- instance Atom ECOQuadOverLin where
  --   vexity = Convex
  --   monotonicity [s1,s2]
  --    | s1==Positive -> [Increasing, Decreasing]
  --    | otherwise -> [Nonmonotone, Decreasing]
  --   sign _ = Positive
  
  -- Expression tree might look like
  -- Node (Op ECOSQuadOverLin) (Leaf a) (Leaf b)
  
    -- isPositive, isNegative
    
    -- apply :: a -> a
    -- rewrite
    -- value :: a -> Double
    -- vexity :: a -> Vexity
    -- montonicity :: a -> Monotonicity
  

  
  -- instance Function Operation where
  --   monotonicity (BinaryOp s) sign = [Increasing, Increasing]
  --   monotonicity (UnaryOp s) sign = [Increasing]
  -- 
  -- instance Vexity Operation where
  --   vexity _ = Affine
  --   sign _ = Unknown
  

    
  
  -- instance Vexity Blah where
  --   isConvex A = True
  --   isConcave A = False
  --   isConvex B = True
  --   isConcave B = True
  --   isConvex _ = True
  --   isConcave _ = True
  -- 
  
      
  -- testme1 :: Expression -> [Bool]
  -- testme1 (Leaf node) = [(isConvex node) || (isConcave node)]
  -- testme1 (Unary op rest) = False:testme1 rest
  -- testme1 (Binary op left right) = (testme1 left) ++ [False] ++ (testme1 right)
  -- eval operation arg
  -- eval Mul 4.0  => (4.0*)
  
  -- tree is somehow easier to deal with than RPN (abstractly, at least)
  -- RPN is easier for rewriting
  -- you just gobble operands and rewrite as needed
  -- 
  -- RPN for vexity inference?
  -- tree for vexity inference that produces RPN?
  --
  -- things that can be.... "eval'd"? or "vex'd"?
  -- an Expression is something that can be vex'd
  -- given any Expression, i can determine if it's convex or concave or neither
  -- Expression is stored as RPN
  
  -- a Tree lets you test vexity pretty quickly, an RPN lets you get the results you need
  -- one possibility is to test DCP on the Tree and, in the process, convert it to RPN
  -- another is to keep it in RPN form and return DCP *as well* as rewrite the problem