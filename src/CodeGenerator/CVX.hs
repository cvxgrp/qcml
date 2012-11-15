module CodeGenerator.CVX (cvxgen) where
  import CodeGenerator.Common
    
{-- what follows is for displaying cvx code --}
  cvxgen :: Codegen -> String
  cvxgen p = let x = problem p
                 problemSense
                  | sense x == Minimize = "minimize "
                  | sense x == Maximize = "maximize "
                  | sense x == Find = "find "
    in unlines $ ["cvx_begin",
      "variables " ++ (unwords $ getVariables x),
      problemSense ++ (name.obj) x] ++
      getRowConstraints x ++
      getConeConstraints x ++
      [ "cvx_end",
        "ecos_optval = cvx_optval;"]
        
  -- gets the variables (and their sizes) from the problem
  getVariables :: SOCP -> [String]
  getVariables p = 
    let extractInfo var = name var ++ "(" ++ (show $ rows var) ++ ")" 
    in map extractInfo (getVariableInfo p)
    
  -- gets string representation of coeff
  getCoeffString :: Coeff -> String
  getCoeffString x = let (m,n,s) = getCoeffInfo x
    in s
  
  -- converts the row constraints in to a string for CVX
  getRowConstraints :: SOCP -> [String]
  getRowConstraints p =
    let coeffStrings = map getCoeffString (affine_b p)
    in produceEqConstr coeffStrings (map convertRow (affine_A p))

  -- adds coefficient for multiply on each row
  convertRow :: Row -> ([String],Bool)
  convertRow row = let coefficients = coeffs row
                       rowHeights = map coeffRows (tail coefficients)
                       rowTotal = coeffRows (head coefficients)
                       isConcat = not $ all (==rowTotal) rowHeights
    in (map (\(multiplier, variable) -> let (m,n,s) = getCoeffInfo multiplier
      in s ++ "*" ++ (name variable)) (elems row), isConcat)

  -- produces an equality constraint
  produceEqConstr :: [String]->[([String],Bool)]->[String]
  produceEqConstr b ax = 
    let zipFunc x y = x ++ " == " ++ y
    in zipWith zipFunc (flattenEqConstr ax) b

  -- flattens the lhs of each row by adding "+"
  flattenEqConstr :: [([String],Bool)] -> [String]
  flattenEqConstr [([],_)] = [""] 
  flattenEqConstr ax = let axs = map fst ax
                           concats = map snd ax
                      in zipWith emitConstraint concats axs

  -- emits a row of the equality constraints
  emitConstraint :: Bool -> [String] -> String
  emitConstraint False x = intercalate " + " x
  emitConstraint True x 
    = (head x) ++ " + " ++ "["++(intercalate "; " (tail x)) ++"]"

  -- gets the cone constraints
  getConeConstraints :: SOCP -> [String]
  getConeConstraints p = map convertCone (cones_K p)

  -- converts cone constraints to CVX string
  convertCone :: SOC -> String
  convertCone (SOC vars) = case (length vars) of
    1 -> (name $ head vars) ++ " >= 0"
    otherwise -> "norm([" ++ (intercalate ", " (map name (tail vars))) ++ "]) <= " ++ (name $ head vars)
  convertCone (SOCelem vars) = case (length vars) of
    1 -> (name $ head vars) ++ " >= 0"
    otherwise -> "norms([" ++ (intercalate ", " (map name (tail vars))) ++ "], [], 2) <= " ++ (name $ head vars)
