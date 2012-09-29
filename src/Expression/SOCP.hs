module Expression.SOCP (VarId(..), Row(..), Problem(..), SOC(..)) where
  -- for indexing in to the matrix
  data VarId = VarId { label :: String } deriving (Eq,Show)

  -- a row in the A matrix
  type Row = [(VarId, String)]

  data Problem = Problem {
    obj :: VarId,     -- objective is always just a single variable
    matrixA :: [Row],
    vectorB :: [Double], 
    conesK :: [SOC]
    }
    deriving (Show)

  data SOC = SOC1 { variables :: [VarId] } 
    | SOC2 { variables :: [VarId] } 
    | SOC3 { variables :: [VarId] }
    deriving (Show)