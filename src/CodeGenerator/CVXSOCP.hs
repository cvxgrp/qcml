module CodeGenerator.CVXSOCP (codegen) where
  import CodeGenerator.Common
  
  {-- what follows is for codegen --}
  codegen :: Problem -> Int -> String
  codegen p c = let vars = getVariableNames p
                    indices = [1..length vars]  -- indices change for C code
                    varTable = zip vars indices
                    n = show $ length vars
                    m = show $ length (vectorB p)
                    csign = show c
    in unlines $ ["c_ = sparse(" ++ n ++ ",1);",
      "c_(" ++ n ++ ") = " ++ csign ++ ";",
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
    ++ ["ecos_optval = "++csign++"*cvx_optval;"]

  -- gets the cone constraints
  getConeConstraintsForCodegen :: Problem -> [(String,Int)]->[String]
  getConeConstraintsForCodegen p table = map (convertConeForCodegen table) (conesK p)
  
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