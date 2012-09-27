module Expression.Expression (Monotonicity'(..), 
  Vexity'(..), 
  Sign'(..), 
  Expression(..), 
  Vexity(..), 
  Function(..), 
  isConvex,
  isConcave,
  isAffine,
  isPositive,
  isNegative) where
  -- TODO: eliminate distinction between 1 and 2 argument operations

  -- quad over lin x y = minimize t subject to ((y+t)/2, (y-t)/2, x) in SOC3, y in SOC1
  
  data Monotonicity' = Increasing 
    | Decreasing 
    | Nonmonotone 
    deriving (Show, Eq)
  data Vexity' = Convex 
    | Concave 
    | Affine 
    | Nonconvex 
    deriving (Show, Eq)
  data Sign' = Positive
    | Negative
    | Unknown
    deriving (Show, Eq)
  
  -- A function must have a vexity defined
  class Function a where
    monotonicity :: a -> [Sign'] -> [Monotonicity']
  
  -- All expressions must have a vexity
  class Vexity a where
    vexity :: a -> Vexity'
    sign :: a -> Sign'
  
  -- An expression is made up of operations *and* literals
  data Expression a b = Nil
    | Leaf b
    | Binary a (Expression a b) (Expression a b)
    | Unary a (Expression a b)
    deriving (Show)
  
  -- helper functions
  isConvex :: Vexity a => a -> Bool
  isConvex x = not (vexity x == Concave)

  isConcave :: Vexity a => a -> Bool
  isConcave x = not (vexity x == Convex)

  isAffine :: Vexity a => a -> Bool
  isAffine x = isConvex x && isConcave x

  isPositive :: Vexity a => a -> Bool
  isPositive x = (sign x) == Positive

  isNegative :: Vexity a => a -> Bool
  isNegative x = not (isPositive x)
    
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