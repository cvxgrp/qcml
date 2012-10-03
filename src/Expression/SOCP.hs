module Expression.SOCP (VarId(..), Row(..), Problem(..), SOC(..), objVar, objLabel) where
  import Data.List
  
  -- for indexing in to the matrix
  data VarId = VarId { label :: String } deriving (Eq,Show)

  -- a row in the A matrix
  type Row = [(VarId, String)]

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
    Nothing -> VarId "0"  -- not really right...
    Just y -> y
  
  -- gets the list of unique variable names for CVX
  getVariableNames :: Problem -> [String]
  getVariableNames p = 
    let objVar = objLabel p
        aVars = (map (label.fst) (concat (matrixA p)))
        coneVars = (map label (concat $ map variables (conesK p)))
    in nub ([objVar] ++ aVars ++ coneVars)
  
  -- converts the row constraints in to a string for CVX
  getRowConstraints :: Problem -> [String]
  getRowConstraints p = produceEqConstr (vectorB p) (map convertRow (matrixA p))
  
  -- adds coefficient for multiply on each row
  convertRow :: Row -> [String]
  convertRow row = map 
    (\(variable, multiplier) -> multiplier ++ "*" ++ (label variable)) row
  
  -- produces an equality constraint
  produceEqConstr :: [String]->[[String]]->[String]
  produceEqConstr b ax = zipWith (\x y -> x ++ " == " ++ y) 
    (flattenEqConstr ax) b
  
  -- flattens the lhs of each row by adding "+"
  flattenEqConstr :: [[String]] -> [String]
  flattenEqConstr [[]] = [""] 
  flattenEqConstr ax = map (foldl1 (\x y -> x ++ " + " ++ y)) ax
  
  -- gets the cone constraints
  getConeConstraints :: Problem -> [String]
  getConeConstraints p = map convertCone (conesK p)
  
  -- converts cone constraints to CVX string
  convertCone :: SOC -> String
  convertCone (SOC vars) = case (length vars) of
    1 -> (label $ head vars) ++ " >= 0"
    otherwise -> "norm([" ++ (intercalate ", " (map label (tail vars))) ++ "]) <= " ++ (label $ head vars)
  
  -- define what happens when we display a problem
  instance Show Problem where
    show EmptyProblem = "Attempted to show an empty problem (likely because it's nonconvex)...."
    show x = unlines $ ["cvx_begin",
      "variables " ++ (unwords $ getVariableNames x),
      "minimize " ++ objLabel x] ++
      getRowConstraints x ++
      getConeConstraints x ++
      ["cvx_end"]

