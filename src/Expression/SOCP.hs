module Expression.SOCP (VarId(..), Coeff(..), Row(..), Problem(..), SOC(..), objVar, objLabel) where
  import Data.List

  -- for indexing in to the matrix
  data VarId = VarId { 
                label :: String, 
                rows :: Int, 
                cols :: Int 
              }
  
  -- for creating coefficients
  -- note that Eye (1) and Ones (1) do the same thing
  -- Eye is a diagonal matrix, Ones is an array
  -- the double stores the *value* of the coefficient
  -- XXX. at the moment, don't need more than *one* value for entire coefficient
  data Coeff = Eye Int String   -- eye matrix
      | Ones Int String         -- ones vector
      | OnesT Int String        -- ones' row vector
      | Matrix (Int, Int) String  -- generic matrix
      | Vector Int String         -- generic vector

  
  -- for showing VarId
  instance Show VarId where
    show x = label x ++ "("++(show $ rows x)++", "++(show $ cols x)++")"

  -- a row in the A matrix, the string gives the name of the coefficient
  type Row = [(VarId, Coeff)]

  -- XXX: a Problem should have a sense....
  -- SOCP problem data type
  data Problem = Problem {
    obj :: Maybe VarId,     -- objective is always just a single variable
    matrixA :: [Row],
    vectorB :: [Coeff], 
    conesK :: [SOC]
    } | EmptyProblem
    
  -- differentiate between SOC and elementwise SOC
  -- SOC [x,y,z] means norm([y,x]) <= z
  -- SOCelem [x,y,z] means norms([x y]')' <= z
  -- note that SOC [x] and SOCelem [x] both mean x >= 0
  -- XXX. they should mean the same, but code generation treats them differently :()
  data SOC = SOC { variables :: [VarId] } 
    | SOCelem { variables :: [VarId] }
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
