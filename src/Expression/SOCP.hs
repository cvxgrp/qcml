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
  -- starts with cone vars
  -- objective var is at the end
  getVariableNames :: Problem -> [String]
  getVariableNames p = 
    let objVar = objLabel p
        aVars = (map (label.fst) (concat (matrixA p)))
        coneVars = (map label (concat $ map variables (conesK p)))
        uniqueVarNames = nub ([objVar] ++ coneVars ++ aVars)
    in (tail uniqueVarNames) ++ [head uniqueVarNames]

  
  -- define what happens when we display a problem (gives CVX output)
  instance Show Problem where
    show EmptyProblem = "Attempted to show an empty problem (likely because it's nonconvex)...."
    show x = codegenECOS x -- codegenECOS x -- codegenECOS x

{-- what follows is for displaying cvx code --}
  cvxgen :: Problem -> String
  cvxgen x = unlines $ ["cvx_begin",
    "variables " ++ (unwords $ getVariableNames x),
    "minimize " ++ objLabel x] ++
    getRowConstraints x ++
    getConeConstraints x ++
    ["cvx_end"]
  
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
  flattenEqConstr ax = map (intercalate " + ") ax

  -- gets the cone constraints
  getConeConstraints :: Problem -> [String]
  getConeConstraints p = map convertCone (conesK p)

  -- converts cone constraints to CVX string
  convertCone :: SOC -> String
  convertCone (SOC vars) = case (length vars) of
    1 -> (label $ head vars) ++ " >= 0"
    otherwise -> "norm([" ++ (intercalate ", " (map label (tail vars))) ++ "]) <= " ++ (label $ head vars)

{-- what follows is for codegen --}
  codegen :: Problem -> String
  codegen p = let vars = getVariableNames p
                  indices = [1..length vars]  -- indices change for C code
                  varTable = zip vars indices
                  n = show $ length vars
                  m = show $ length (vectorB p)
    in unlines $ ["c_ = sparse(" ++ n ++ ",1);",
      "c_(" ++ n ++ ") = 1;",
      "b_ = sparse(" ++ m ++ ",1);",
      getBForCodegen p ++
      "A_ = sparse(" ++ m ++ ", " ++ n ++ ");",
      getAForCodegen p varTable,
      "cvx_begin",
      "variable x_codegen(" ++ n ++ ")",
      "minimize (c_'*x_codegen)",
      "A_*x_codegen == b_"
    ] ++ getConeConstraintsForCodegen p varTable ++
    ["cvx_end"] ++ socpToProb varTable

  codegenECOS :: Problem -> String
  codegenECOS p = let vars = getVariableNames p
                      indices = [1..length vars]  -- indices change for C code
                      varTable = zip vars indices
                      n = show $ length vars
                      m = show $ length (vectorB p)
                      (k, g, cones) = getConeConstraintsForECOS p varTable
                      nk = show (length vars - k)
                      kshow = show k
    in unlines $ ["c_ = sparse(" ++ n ++ ",1);",
      "c_(" ++ n ++ ") = 1;",
      "b_ = sparse(" ++ m ++ ",1);",
      getBForCodegen p ++
      "A_ = sparse(" ++ m ++ ", " ++ n ++ ");",
      getAForCodegen p varTable,
      -- for ecos call
      "G_ = sparse("++kshow++", " ++ n ++ ");\n" ++
      g ++
      "h_ = zeros("++ kshow ++ ", 1);",
      cones,
      "[x_codegen, y_, info_] = paris(full(c_), G_, h_, dims, A_, full(b_));"
    ] ++ socpToProb varTable
    
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
    
  -- gets the cone constraints
  getConeConstraintsForCodegen :: Problem -> [(String,Int)]->[String]
  getConeConstraintsForCodegen p table = map (convertConeForCodegen table) (conesK p)
  
  -- gets the dimensions for cone constraints
  getConeConstraintsForECOS :: Problem -> [(String,Int)]->(Int, String, String)
  getConeConstraintsForECOS p table = 
    let coneVariables = map coneVar (conesK p)
        coneSizes = map length coneVariables
        -- generate permutation vector
        permute = (\x -> map snd $ sort $ zip x [1..(length x)]) coneSizes
        permuteVars = permuteVariables permute coneVariables
        matrixG = createMatrixG (concat permuteVars) 1 table
        m = foldl (+) 0 coneSizes
        higherDimCones = sort $ filter (>1) coneSizes
        l = m - (foldl (+) 0 higherDimCones)
    in (m, matrixG, "dims.q = " ++ show higherDimCones ++ ";\ndims.l = " ++ show l ++ ";")
  

  -- permute the variables
  permuteVariables :: [Int]->[[VarId]]->[[VarId]]
  permuteVariables [] _ = []
  permuteVariables _ [] = []
  permuteVariables (p:ps) xs =
    let val = xs!!(p-1)
    in val:permuteVariables ps xs

  createMatrixG :: [VarId] -> Int -> [(String,Int)] -> String
  createMatrixG [] _ _ = ""
  createMatrixG (x:xs) count table = 
    let newNames = (flip lookup table) (label x)
        ind = case (newNames) of
          Nothing -> ""
          Just x -> show x
    in "G_("++ show count ++", " ++ ind ++ ") = -1;\n" 
      ++ createMatrixG xs (count+1) table
  
  -- gets the variables in the cone
  coneVar :: SOC -> [VarId]
  coneVar (SOC vars) = vars

  -- converts cone constraints to CVX string
  convertConeForCodegen :: [(String,Int)] -> SOC -> String
  convertConeForCodegen table (SOC vars) = 
    let newNames = map (flip lookup table) (map label vars)
    in case (length vars) of
      1 -> (extractString $ head newNames) ++ " >= 0"
      otherwise -> "norm([" ++ (intercalate ", " (map extractString (tail newNames))) ++ "]) <= " ++ (extractString $ head newNames)
      
  extractString :: Maybe Int -> String
  extractString Nothing = ""
  extractString (Just x) = "x_codegen(" ++ show x ++ ")"
    