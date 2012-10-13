module Expression.SOCP (VarId(..), Row(..), Problem(..), SOC(..), objVar, objLabel) where
  import Data.List

  -- for indexing in to the matrix
  data VarId = VarId { 
                label :: String, 
                rows :: Int, 
                cols :: Int 
              }
  
  -- for showing VarId
  instance Show VarId where
    show x = label x ++ "("++(show $ rows x)++", "++(show $ cols x)++")"
              
  -- instance Eq VarId where
  --   x == y  = (label x) == (label y)

  -- a row in the A matrix, the string gives the name of the coefficient
  type Row = [(VarId, String)]

  -- XXX: a Problem should have a sense....
  -- SOCP problem data type
  data Problem = Problem {
    obj :: Maybe VarId,     -- objective is always just a single variable
    matrixA :: [Row],
    vectorB :: [String], 
    conesK :: [SOC]
    } | EmptyProblem
    
  data SOC = SOC { variables :: [VarId] } 
    deriving (Show)
  
  -- gets the label on the objective
  objLabel :: Problem -> String
  objLabel x = case (obj x) of
    Nothing -> "0"
    Just y -> label y
  
  -- gets the variable in the objective
  objVar :: Problem -> VarId
  objVar x = case (obj x) of
    Nothing -> VarId "0" 1 1  -- not really right...
    Just y -> y
