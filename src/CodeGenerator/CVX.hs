module CodeGenerator.CVX (cvxgen) where
  import CodeGenerator.Common
    
{-- what follows is for displaying cvx code --}
  cvxgen :: Problem -> Int -> String
  cvxgen EmptyProblem _ = "0";
  cvxgen x c = let problemSense = case(c >= 0) of
                    True -> "minimize "
                    False -> "maximize "
    in unlines $ ["cvx_begin",
      "variables " ++ (unwords $ getVariableNames x),
      problemSense ++ objLabel x] ++
      getRowConstraints x ++
      getConeConstraints x ++
      [ "cvx_end",
        "ecos_optval = cvx_optval;"]
  
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
