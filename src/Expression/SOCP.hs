module Expression.SOCP (
  Sense(..), Var(..), vrows, vcols,
  Row, Coeff(..), (<++>),
  Cones(..), SOC(..), SOCP(..)) where

  -- problem sense
  data Sense = Maximize | Minimize | Find deriving (Eq, Show)

  -- variables
  -- TODO: to handle constant folding, introduce a "Const" object in addition to Var
  data Var = Var {
    vname:: String,  
    vshape:: (Int, Int)
  } deriving (Show)

  vrows x = (fst.vshape) x
  vcols x = (snd.vshape) x
  
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
      deriving (Show)
  
  -- a row in the A matrix
  type Row = [(Coeff, Var)]

  data Cones = Cones {
    matrixA :: [Row],
    vectorB :: [Coeff],
    conesK :: [SOC]
  } deriving (Show)

  (<++>) :: Cones -> Cones -> Cones
  x <++> y = Cones (matrixA x ++ matrixA y) (vectorB x ++ vectorB y) (conesK x ++ conesK y)

  data SOCP = SOCP {
    sense :: Sense,
    obj :: Var, -- objective is always just a single variable
    constraints :: Cones
  } deriving (Show)


  -- differentiate between SOC and elementwise SOC
  -- SOC [x,y,z] means norm([y,x]) <= z
  -- SOCelem [x,y,z] means norms([x y]')' <= z
  -- note that SOC [x] and SOCelem [x] both mean x >= 0
  data SOC = SOC { variables :: [Var] } 
    | SOCelem { variables :: [Var] }
    deriving (Show)
    