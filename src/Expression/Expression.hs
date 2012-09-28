module Expression.Expression (Parameter(..), Variable(..), Expression(..)) where
  import DCP.DCP
  
  data Parameter = Parameter deriving (Show)
  data Variable = Variable deriving (Show)
  
  data Node a = Node deriving (Show)
  
  data Expression = Expression deriving (Show)
  
  createNode :: Symbol a => a -> Node a
  createNode _ = Node
  
  instance Symbol Parameter where
    vexity _ = Affine
    sign _ = Unknown
  
  instance Symbol Variable where
    vexity _ = Affine
    sign _ = Unknown
    
  instance Symbol Expression where
    vexity _ = Affine
    sign _ = Unknown
  
  data ECOSPlus = ECOSPlus Expression Expression deriving (Show)
  data ECOSMul = ECOSMul Expression Parameter deriving (Show)
  
  -- Plus atom
  instance Atom ECOSPlus where
    monotonicity _ [Positive,Positive] = [Increasing, Increasing]
    monotonicity _ [Positive,Negative] = [Increasing, Decreasing]
    monotonicity _ [Negative,Positive] = [Decreasing, Increasing]
    monotonicity _ [Negative,Negative] = [Decreasing, Decreasing]
    
    applySign _ [a,b] = case (a, b) of
      (Positive, Positive) -> Positive
      (Negative, Negative) -> Negative
      otherwise -> Unknown
      
  instance Symbol ECOSPlus where
    vexity _ = Affine
    sign (ECOSPlus b c) = applySign (ECOSPlus b c) [sign b, sign c]
  
  -- Multiply atom
  -- error shows up in here!
  -- [b,c], where b is an expression and c is a parameter
  -- need some way to convert parameter and variables to expressions
  instance Atom ECOSMul where
    monotonicity _ [a,b] = case (a) of
      Positive -> [Increasing]
      Negative -> [Decreasing]
      otherwise -> [Nonmonotone]
    applySign _ [a,b] = case (a, b) of
      (Positive, Positive) -> Positive
      (Negative, Negative) -> Positive
      (Positive, Negative) -> Negative
      (Negative, Positive) -> Negative
      otherwise -> Unknown

  instance Symbol ECOSMul where
    vexity _ = Affine
    sign (ECOSMul b c) = applySign (ECOSMul b c) [sign b, sign c]
    
      
  -- it turns out that Parameter, Variables, and Functions are symbols
  -- but Expressions are *also* symbols, i.e., [Symbol] is also a Symbol