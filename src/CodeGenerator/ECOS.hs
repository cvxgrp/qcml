module CodeGenerator.ECOS(codegenECOS, codegenConelp) where
  import CodeGenerator.Common
  
  codegenECOS :: Problem -> Int -> String
  codegenECOS p c = codegen "paris" p c
  
  codegenConelp :: Problem -> Int -> String
  codegenConelp p c = codegen "conelp" p c
  
  codegen :: String -> Problem -> Int -> String
  codegen solver p c = 
    let vars = getVariableNames p
        indices = [1..length vars]  -- indices change for C code 
        varTable = zip vars indices
        n = show $ length vars
        m = show $ length (vectorB p)
        (k, g, cones) = getConeConstraintsForECOS p varTable
        nk = show (length vars - k)
        kshow = show k
        csign = show c
    in unlines $ ["c_ = sparse(" ++ n ++ ",1);",
      "c_(" ++ n ++ ") = " ++ csign ++ ";",
      "b_ = sparse(" ++ m ++ ",1);",
      getBForCodegen p ++
      "A_ = sparse(" ++ m ++ ", " ++ n ++ ");",
      getAForCodegen p varTable,
      "G_ = sparse("++kshow++", " ++ n ++ ");\n" ++
      g ++
      "h_ = zeros("++ kshow ++ ", 1);",
      cones,
      "[x_codegen, y_, info_] = "++solver++"(full(c_), G_, h_, dims, A_, full(b_));"
    ] ++ socpToProb varTable
    ++ ["ecos_optval = "++csign++"*info_.pcost;"]
    -- dump data to a header file to call ecos c code...
    ++ ["cg_dump_conelpproblem(c_,G_, h_, dims, A_, b_, 'data.h');"]
  
  -- gets the dimensions for cone constraints
  getConeConstraintsForECOS :: Problem -> [(String,Int)]->(Int, String, String)
  getConeConstraintsForECOS p table = 
   let coneVariables = map coneVar (conesK p)
       coneSizes = map coneLength coneVariables
       -- generate permutation vector (put smallest cones up front)
       permute = (\x -> map snd $ sort $ zip x [1..(length x)]) coneSizes
       -- permuted cone variables
       permuteVars = concat (permuteVariables permute coneVariables)
       matrixG = createMatrixG permuteVars 1 table
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
   let newNames = (flip lookup table) (label x) -- lookup index
       ind = case (newNames) of
         Nothing -> ""
         Just x -> show x
   in "G_("++ show count ++", " ++ ind ++ ") = -1;\n" 
     ++ createMatrixG xs (count+1) table

  -- gets the variables in the cone
  coneVar :: SOC -> [VarId]
  coneVar (SOC vars) = vars

  -- gets the size of the cone by looking at the variables' sizes in the cone
  coneLength :: [VarId] -> Int
  coneLength x = let varSizes = map (\x -> (rows x)*(cols x)) x
    in foldl (+) 0 varSizes

