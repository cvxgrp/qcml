module CodeGenerator.Common (
  getVariableNames, 
  getVariableRows,
  getBRows,
  getVariableInfo,
  socpToProb, 
  getAForCodegen,
  getBForCodegen,
  VarTable,
  Codegen(..),
  getCoeffInfo,
  getCoeffSize,
  coeffRows,
  cones_K, affine_A, affine_b,
  cumsum,
  module Expression.SOCP,
  module Data.List) where

  import Expression.SOCP
  import Expression.Expression(Expr)
  import Data.List
  import qualified Data.Map as M


  data Codegen = Codegen {
    problem :: SOCP,
    symbolTable :: M.Map String Expr
  }

  -- XXX/TODO: code generator is long overdue for a rewrite

  -- helper functions
  cones_K = conesK.constraints
  affine_A = matrixA.constraints
  affine_b = vectorB.constraints

  cumsum :: Num a => [a] -> a
  cumsum = foldl' (+) 0

  -- a VarTable is an associatiation list with (name, (start, len))
  type VarTable = [(String, (Int,Int))]
  
  getVariableNames :: SOCP -> [String]
  getVariableNames p = map name (getVariableInfo p)
  
  getVariableRows :: SOCP -> [Int]
  getVariableRows p = map rows (getVariableInfo p)

  getBRows :: SOCP -> [Int]
  getBRows p = map coeffRows (affine_b p) 
  
  -- gets the list of unique variable names for CVX
  -- starts with cone vars
  -- objective var is at the end
  getVariableInfo :: SOCP -> [Var]
  getVariableInfo p = 
    let objectiveVar = obj p
        aVars =  concat $ map variables (affine_A p)  -- gets the list of all variables in the affine constraints
        coneVars = concat $ map variables (cones_K p) -- get the list of all variables in the cones_K
        allVariables = [objectiveVar] ++ coneVars ++ aVars
        uniqueVarNames = nubBy (\x y-> name x == name y) allVariables
    in (tail uniqueVarNames) ++ [head uniqueVarNames]
    
  -- write out results
  socpToProb :: VarTable -> [String]
  socpToProb table = map (\(s,(i,l)) -> s ++ " = x_codegen(" ++ show i ++ ":" ++ show (i+l-1) ++ ");") table
  
  -- get A
  getAForCodegen = getAForCodegenWithIndx 1
  getAForCodegenC = getAForCodegenWithIndx 0

  getAForCodegenWithIndx :: Int -> SOCP -> VarTable -> String
  getAForCodegenWithIndx i p table = 
    let bsizes = map coeffRows (affine_b p)  -- height of each row
        startIdx = take (length bsizes) (scanl (+) i bsizes)  -- gives start index for each row
    in intercalate "\n" (map (createRow table) (zip (affine_A p) startIdx))
  
  createRow :: VarTable -> (Row,Int) -> String
  createRow table (row,ind) = 
    let indices = map (flip lookup table) (varnames row)
        coefficients = coeffs row
        -- with our setup, the only time rowHeights aren't equal to the first one is when we are concatenating
        rowHeights = map coeffRows (tail coefficients)
        rowTotal = coeffRows (head coefficients)
        offsets
          | all (==rowTotal) rowHeights = 0:(map (rowTotal-) rowHeights)
          | otherwise = 0:rowHeights
        shifts = init $ scanl (+) 0 offsets
    in intercalate " " (zipWith (assignToA ind) shifts (zip (elems row) indices))
    
  -- get coeff size and value
  getCoeffInfo :: Coeff -> (Int,Int,String)
  getCoeffInfo (Matrix p) = (rows p, cols p, name p)
  getCoeffInfo (MatrixT p) = (cols p, rows p, (name p) ++ "'")
  getCoeffInfo (Vector n p) = (n,1, name p)
  getCoeffInfo (VectorT n p) = (n,1, (name p)++"'")
  getCoeffInfo (Diag 1 p) = (1,1, name p)
  getCoeffInfo (Diag n p) = (n,n,"spdiags("++ name p ++ ",0,"++show n++","++show n++")")
  getCoeffInfo (OnesT n 0) = (1,n, "0")
  getCoeffInfo (OnesT 1 x) = (1,1, show x)
  getCoeffInfo (OnesT n x) = (1,n, (show x)++"*ones(1, "++show n ++")")
  getCoeffInfo (Ones n 0) = (n,1, "0")
  getCoeffInfo (Ones 1 x) = (1,1, show x)
  getCoeffInfo (Ones n x) = (n,1, (show x)++"*ones("++show n ++", 1)")
  getCoeffInfo (Eye n 0) = (n,n,"0")
  getCoeffInfo (Eye 1 x) = (1,1, show x)
  getCoeffInfo (Eye n x) = (n,n,(show x) ++ "*speye("++show n++", "++show n++")")
  
  -- just get coeff size
  getCoeffSize :: Coeff -> (Int, Int)
  getCoeffSize x = let (m,n,s) = getCoeffInfo x
    in (m,n)
    
  -- just get coeff rows
  coeffRows :: Coeff -> Int
  coeffRows x = let (m,n,s) = getCoeffInfo x
    in m
  
  -- XXX: this is such a bizarre function type signature...
  assignToA :: Int -> Int -> ((Coeff, Var), Maybe (Int,Int)) -> String
  assignToA _ _ (_, Nothing) = ""
  assignToA x offset (row, Just (y,l)) = 
    let (m,n,val) = getCoeffInfo (fst row) -- n should equal l at this point!!
        rowExtent = show (x+offset) ++ ":" ++ show (x+offset+m-1)
        colExtent = show y ++ ":" ++ show (y+n-1)
    in case(val) of
      "0" -> ""
      otherwise -> "A_(" ++ rowExtent ++ ", " ++ colExtent ++ ") = " ++ val ++ ";"
  
  -- get b
  getBForCodegen = getBForCodegenWithIndx 1
  getBForCodegenC = getBForCodegenWithIndx 0

  getBForCodegenWithIndx :: Int -> SOCP -> String
  getBForCodegenWithIndx i p = 
    let b = affine_b p
        sizes = map coeffRows b
        startIdx = init $ scanl (+) i sizes -- start index changes for C code
    in concat $ map assignToB (zip b startIdx)
  
  assignToB :: (Coeff,Int) -> String
  assignToB (val, ind) = 
    let (m,n,s) = getCoeffInfo val
    in case (s) of
      "0" -> ""
      otherwise -> "b_("++ (show ind) ++ ":"++ show (ind+m-1)++") = " ++ s ++ ";\n"