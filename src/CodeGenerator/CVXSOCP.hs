module CodeGenerator.CVXSOCP (codegen) where
  import CodeGenerator.Common
  
  {-- what follows is for codegen --}
  codegen :: SOCP -> String
  codegen p = let vars = getVariableNames p
                  varLens = getVariableSizes p
                  startIdx = take (length vars) (scanl (+) 1 varLens)  -- indices change for C code
                  varTable = zip vars (zip startIdx varLens)
                  n = show $ (foldl (+) 0 varLens)
                  bsizes = map (fst.getCoeffSize) (affine_b p)
                  m = show $ foldl (+) 0 bsizes
                  csign
                    | sense p == Minimize = "1"
                    | sense p == Maximize = "-1"
                    | sense p == Find = "0"
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
  getConeConstraintsForCodegen :: SOCP -> VarTable ->[String]
  getConeConstraintsForCodegen p table = map (convertConeForCodegen table) (cones p)
  
  -- converts cone constraints to CVX string
  convertConeForCodegen :: VarTable -> SOC -> String
  convertConeForCodegen table (SOC vars) = 
   let newNames = map (flip lookup table) (map vname vars)
   in case (length vars) of
     1 -> (extractString $ head newNames) ++ " >= 0"
     otherwise -> "norm([" ++ (intercalate ", " (map extractString (tail newNames))) ++ "]) <= " ++ (extractString $ head newNames)
  convertConeForCodegen table (SOCelem vars) = 
    let newNames = map (flip lookup table) (map vname vars)
    in case (length vars) of
      1 -> (extractString $ head newNames) ++ " >= 0"
      otherwise -> "norms([" ++ (intercalate ", " (map extractString (tail newNames))) ++ "],[],2) <= " ++ (extractString $ head newNames)

  extractString :: Maybe (Int,Int) -> String
  extractString Nothing = ""
  extractString (Just (x,l)) = "x_codegen(" ++ show x ++ ":" ++ show (x+l-1) ++ ")"