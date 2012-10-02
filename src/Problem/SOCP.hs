module Problem.SOCP (VarId(..), Row(..), Problem(..), SOC(..), getRowConstraints) where
  import Data.List
  
  -- for indexing in to the matrix
  data VarId = VarId { label :: String } deriving (Eq,Show)

  -- a row in the A matrix
  type Row = [(VarId, String)]

  data Problem = Problem {
    obj :: VarId,     -- objective is always just a single variable
    matrixA :: [Row],
    vectorB :: [String], 
    conesK :: [SOC]
    } | EmptyProblem
    
  data SOC = SOC1 { variables :: [VarId] } 
    | SOC2 { variables :: [VarId] } 
    | SOC3 { variables :: [VarId] }
    deriving (Show)
    
  getVariableNames :: Problem -> [String]
  getVariableNames p = nub $ (label $ obj p):(map (label.fst) (concat (matrixA p))) ++ (map label (concat $ map variables (conesK p)))
  
  getRowConstraints :: Problem -> [String]
  getRowConstraints p = produceEqConstr (vectorB p) (map convertRow (matrixA p))
  
  convertRow :: Row -> [String]
  convertRow r = map (\x -> (snd x) ++ "*" ++ (label.fst) x) r
  
  produceEqConstr :: [String]->[[String]]->[String]
  produceEqConstr b ax = zipWith (\x y -> x ++ " ==" ++ y) 
    b (flattenEqConstr ax)
    
  flattenEqConstr :: [[String]] -> [String]
  flattenEqConstr ax = map (foldl (\x y -> x ++ " + " ++ y) "") ax
  
  getConeConstraints :: Problem -> [String]
  getConeConstraints p = map convertCone (conesK p)
  
  convertCone :: SOC -> String
  convertCone (SOC1 vars) = (label $ head vars) ++ " >= 0"
  convertCone (SOC2 vars) = "norm([" ++ (intercalate ", " (map label (tail vars))) ++ "]) <= " ++ (label $ head vars)
  convertCone (SOC3 vars) = "norm([" ++ (intercalate ", " (map label (tail vars))) ++ "]) <= " ++ (label $ head vars)
  
  -- define what happens when we display a problem
  instance Show Problem where
    show EmptyProblem = "Attempted to show an empty problem (likely because it's nonconvex)...."
    show x = unlines $ ["variables " ++ (unwords $ getVariableNames x),
      "minimize " ++ (label.obj) x] ++
      getRowConstraints x ++
      getConeConstraints x

