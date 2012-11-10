module Expression.SOCP (
  Sense(..), Var(Var), Param(Param,transposed), Symbol(..),
  Row(..), Coeff(..), (<++>),
  ConicSet(..), SOC(..), SOCP(..), VarList(..), coeffs) where

  data SOCP = SOCP {
    sense :: Sense,
    obj :: Var, -- objective is always just a single variable
    constraints :: ConicSet
  } deriving (Show)

  -- problem sense
  data Sense = Maximize | Minimize | Find deriving (Eq, Show)

  -- variables
  -- TODO: to handle constant folding, introduce a "Const" object in addition to Var
  data Var = Var {
    vname:: String,  
    vshape:: (Int, Int)
  } deriving (Show)

  data Param = Param {
    pname :: String,
    -- psign :: Sign, -- only include sign if we need to check it in codegen
    pshape :: (Int, Int),
    transposed :: Bool
  } deriving (Show)

  class Symbol a where
    rows :: a -> Int
    cols :: a -> Int
    shape :: a -> (Int, Int)
    name :: a -> String

  instance Symbol Var where
    rows = fst.vshape
    cols = snd.vshape
    shape = vshape
    name = vname

  instance Symbol Param where
    rows = fst.pshape
    cols = snd.pshape
    shape = pshape
    name = pname
  
  -- for creating coefficients
  -- divided in to constants (Eye, Ones, OnesT)
  -- and parameters (All, Diag, Matrix, Vector)
  --
  -- note that Eye (1) and Ones (1) do the same thing
  -- Eye is a diagonal matrix, Ones is an array
  -- the double stores the *value* of the coefficient
  -- XXX. at the moment, don't need more than *one* value for entire coefficient
  data Coeff = Eye Int Double   -- eye matrix
      | Ones Int Double         -- ones vector
      | OnesT Int Double        -- ones' row vector
      | Diag Int Param          -- replicate a parameter to diag(s) matrix
      | Matrix Param            -- generic matrix
      | Vector Int Param        -- generic vector
      deriving (Show)
  
  data ConicSet = ConicSet {
    matrixA :: [Row],
    vectorB :: [Coeff],
    conesK :: [SOC]
  } deriving (Show)

  (<++>) :: ConicSet -> ConicSet -> ConicSet
  x <++> y = ConicSet (matrixA x ++ matrixA y) (vectorB x ++ vectorB y) (conesK x ++ conesK y)


  -- differentiate between SOC and elementwise SOC
  -- SOC [x,y,z] means norm([y,x]) <= z
  -- SOCelem [x,y,z] means norms([x y]')' <= z
  -- note that SOC [x] and SOCelem [x] both mean x >= 0
  data SOC = SOC { vars :: [Var] } 
    | SOCelem { vars :: [Var] }
    deriving (Show)

  -- TODO: we can make a "concat" row type?
  -- TODO: we can include the row height in here
  data Row = Row { elems :: [(Coeff,Var)] } deriving (Show)

  -- type class for accessing variables
  class VarList a where
    variables :: a -> [Var]
    varnames :: a -> [String]

  instance VarList SOC where
    variables = vars
    varnames x = map vname (vars x)

  instance VarList Row where
    variables (Row x) = map snd x
    varnames (Row x) = map (vname.snd) x

  coeffs :: Row -> [Coeff]
  coeffs (Row x) = map fst x