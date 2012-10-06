module CodeGenerator.Common (
  getVariableNames, 
  socpToProb, 
  getAForCodegen,
  getBForCodegen,
  module Expression.SOCP,
  module Data.List) where
  import Expression.SOCP
  import Data.List
  
  -- gets the list of unique variable names for CVX
  -- starts with cone vars
  -- objective var is at the end
  getVariableNames :: Problem -> [String]
  getVariableNames p = 
    let objVar = objLabel p
        aVars = (map (label.fst) (concat (matrixA p)))
        coneVars = (map label (concat $ map variables (conesK p)))
        uniqueVarNames = nub ([objVar] ++ coneVars ++ aVars)
    in (tail uniqueVarNames) ++ [head uniqueVarNames]
    
  -- write out results
  socpToProb :: [(String, Int)] -> [String]
  socpToProb table = map (\(s,i) -> s ++ " = x_codegen(" ++ show i ++ ");") table
  
  -- get A
  getAForCodegen :: Problem -> [(String, Int)] -> String
  getAForCodegen p table = let b = vectorB p
    in intercalate "\n" (map (createRow table) (zip (matrixA p) [1..length b]))
  
  createRow :: [(String, Int)] -> (Row,Int) -> String
  createRow table (row,ind) = 
    let newNames = map (flip lookup table) (map (label.fst) row)
    in intercalate " " (map (assignToA ind) (zip row newNames))
  
  assignToA :: Int -> ((VarId, String), Maybe Int) -> String
  assignToA x (row, Nothing) = ""
  assignToA x (row, Just y) = "A_(" ++ show x ++ ", " ++ show y ++ ") = " ++ (snd row) ++ ";"
  
  -- get b
  getBForCodegen :: Problem -> String
  getBForCodegen p = let b = vectorB p
    in concat $ map assignToB (zip b [1..length b]) -- indices change for C code
  
  assignToB :: (String,Int) -> String
  assignToB ("0", _) = ""
  assignToB (val, ind) = "b_("++ (show ind) ++ ") = " ++ val ++ ";\n"