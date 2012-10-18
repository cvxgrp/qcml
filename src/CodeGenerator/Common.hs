module CodeGenerator.Common (
  getVariableNames, 
  getVariableSizes,
  getVariableInfo,
  socpToProb, 
  getAForCodegen,
  getBForCodegen,
  VarTable,
  getCoeffInfo,
  getCoeffSize,
  getCoeffRows,
  module Expression.SOCP,
  module Data.List) where
  import Expression.SOCP
  import Data.List
  
  -- a VarTable is an associatiation list with (name, (start, len))
  type VarTable = [(String, (Int,Int))]
  
  getVariableNames :: Problem -> [String]
  getVariableNames p = map label (getVariableInfo p)
  
  getVariableSizes :: Problem -> [Int]
  getVariableSizes p = map rows (getVariableInfo p)
  
  -- gets the list of unique variable names for CVX
  -- starts with cone vars
  -- objective var is at the end
  getVariableInfo :: Problem -> [VarId]
  getVariableInfo p = 
    let objectiveVar = objVar p
        aVars = (map (fst) (concat (matrixA p)))
        coneVars = (concat $ map variables (conesK p))
        allVariables = [objectiveVar] ++ coneVars ++ aVars
        uniqueVarNames = nubBy (\x y-> label x == label y) allVariables
    in (tail uniqueVarNames) ++ [head uniqueVarNames]
    
  -- write out results
  socpToProb :: VarTable -> [String]
  socpToProb table = map (\(s,(i,l)) -> s ++ " = x_codegen(" ++ show i ++ ":" ++ show (i+l-1) ++ ");") table
  
  -- get A
  getAForCodegen :: Problem -> VarTable -> String
  getAForCodegen p table = 
    let bsizes = map (fst.getCoeffSize) (vectorB p)
        startIdx = take (length bsizes) (scanl (+) 1 bsizes)
    in intercalate "\n" (map (createRow table) (zip (matrixA p) startIdx))
  
  createRow :: VarTable -> (Row,Int) -> String
  createRow table (row,ind) = 
    let newNames = map (flip lookup table) (map (label.fst) row)
        -- with our setup, the only time rowHeights aren't equal to the last one is when we are concatenating
        rowHeights = map (getCoeffRows.snd) (tail row)
        rowTotal = (getCoeffRows.snd) (head row)
        offsets = case(all (==rowTotal) rowHeights) of
          True -> 0:0:(map (rowTotal-) rowHeights)
          False -> 0:rowHeights
        shifts = init $ scanl (+) 0 offsets
    in intercalate " " (zipWith (assignToA ind) shifts (zip row newNames))
    
  -- get coeff size and value
  getCoeffInfo :: Coeff -> (Int,Int,String)
  getCoeffInfo (Matrix (m,n) s) = (m,n,s)
  getCoeffInfo (Vector n s) = (n,1,s)
  getCoeffInfo (OnesT n s) = case (s) of
    0 -> (1,n,"0")
    otherwise -> (1,n, show s++"*ones(1, "++show n ++")")
  getCoeffInfo (Ones n s) = case (s) of
    0 -> (n,1,"0")
    otherwise -> (n,1, show s++"*ones("++show n ++", 1)")
  getCoeffInfo (Eye n s) = case (s) of
    0 -> (n,n, "0")
    otherwise -> (n,n,show s++"*speye("++show n++", "++show n++")")
  
  -- just get coeff size
  getCoeffSize :: Coeff -> (Int, Int)
  getCoeffSize x = let (m,n,s) = getCoeffInfo x
    in (m,n)
    
  -- just get coeff rows
  getCoeffRows :: Coeff -> Int
  getCoeffRows x = let (m,n,s) = getCoeffInfo x
    in m
  
  assignToA :: Int -> Int -> ((VarId, Coeff), Maybe (Int,Int)) -> String
  assignToA _ _ (_, Nothing) = ""
  assignToA x offset (row, Just (y,l)) = 
    let (m,n,val) = getCoeffInfo (snd row)-- n should equal l at this point!!
        rowExtent = show (x+offset) ++ ":" ++ show (x+offset+m-1)
        colExtent = show y ++ ":" ++ show (y+n-1)
    in case(val) of
      "0" -> ""
      otherwise -> "A_(" ++ rowExtent ++ ", " ++ colExtent ++ ") = " ++ val ++ ";"
  
  -- get b
  getBForCodegen :: Problem -> String
  getBForCodegen p = 
    let b = vectorB p
        sizes = map (fst.getCoeffSize) b
        startIdx = take (length b) (scanl (+) 1 sizes) -- start index changes for C code
    in concat $ map assignToB (zip b startIdx)
  
  assignToB :: (Coeff,Int) -> String
  assignToB (val, ind) = 
    let (m,n,s) = getCoeffInfo val
    in case (s) of
      "0" -> ""
      otherwise -> "b_("++ (show ind) ++ ":"++ show (ind+m-1)++") = " ++ s ++ ";\n"