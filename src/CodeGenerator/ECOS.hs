module CodeGenerator.ECOS(codegenECOS, codegenConelp) where
  import CodeGenerator.Common
  
  codegenECOS :: Codegen -> String
  codegenECOS p = codegen "ecos" (problem p)
  
  codegenConelp :: Codegen -> String
  codegenConelp p = codegen "conelp" (problem p)
  
  codegen :: String -> SOCP -> String
  codegen solver p = 
    let vars = getVariableNames p
        varLens = getVariableSizes p
        startIdx = take (length vars) (scanl (+) 1 varLens)  -- indices change for C code
        varTable = zip vars (zip startIdx varLens)
        n = show $ (foldl (+) 0 varLens)
        bsizes = map (fst.getCoeffSize) (affine_b p)
        m = show $ foldl (+) 0 bsizes
        (k, g, cones) = getConeConstraintsForECOS p varTable
        nk = show (length vars - k)
        kshow = show k
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
      "G_ = sparse("++kshow++", " ++ n ++ ");\n" ++
      g ++
      "h_ = zeros("++ kshow ++ ", 1);",
      cones,
      --"[x_codegen, ind, c1, A1, b1, G1, h1, g_, F_] = ecos_presolve(c_,A_,b_,G_,h_);",
      "[x_codegen, y_, info_] = "++solver++"(full(c_), G_, h_, dims, A_, full(b_));"
      --"x_codegen(ind) = xtmp;",
      --"x_codegen = x_codegen + g_ - F_*x_codegen;",
      --"ind = (x_codegen == 0); tmp = g_ - F_*x_codegen;",
      --"x_codegen(ind) = tmp(ind);"
    ] ++ socpToProb varTable
    ++ ["ecos_optval = "++csign++"*info_.pcost;"]
    -- dump data to a header file to call ecos c code...
    -- ++ ["cg_dump_conelpproblem(c_,G_, h_, dims, A_, b_, 'data.h');"]
  
  -- gets the dimensions for cone constraints
  getConeConstraintsForECOS :: SOCP -> VarTable -> (Int, String, String)
  getConeConstraintsForECOS p table = 
   let coneInfo = map coneVar (cones_K p)
       coneVariables = map fst coneInfo
       coneSizes = map snd coneInfo
       -- generate permutation vector (put smallest cones up front)
       coneGroupSizes = zip (map head coneSizes) [1..(length coneSizes)]
       pvec = map (snd) (sort coneGroupSizes)
       -- get the width of the cones (number of cones to generate per var)
       ncones = map coneWidth coneSizes
       -- permuted cone variables
       permuted = permuteCones pvec ncones coneVariables
       startIdx = getStartIdx 0 permuted
       flattenPermuted = flattenp permuted
       matrixG = createMatrixG flattenPermuted startIdx table
       m = foldl (+) 0 (concat coneSizes)
       higherDimCones = sort $ filter (>1) (concat coneSizes)
       l = m - (foldl (+) 0 higherDimCones)
   in (m, matrixG, "dims.q = " ++ show higherDimCones ++ ";\ndims.l = " ++ show l ++ ";")

  -- permute the cones
  permuteCones :: [Int]->[Int]->[[Var]]->[([Var],Int)]
  permuteCones [] _ _ = []
  permuteCones _ [] _ = []
  permuteCones _ _ [] = []
  permuteCones (p:ps) ns xs  =
   let val = (xs!!(p-1), ns!!(p-1))
   in val:permuteCones ps ns xs

  createMatrixG :: [(Var,Int)] -> [Int] -> VarTable -> String
  createMatrixG [] _ _  = ""
  createMatrixG _ [] _  = ""
  createMatrixG (x:xs) (n:ns) table = 
   let newNames = (flip lookup table) ((name.fst) x) -- lookup index
       (start, l) = case (newNames) of
         Nothing -> (0,0)
         Just x -> x
       rowExtent = show n ++":"++ show (snd x) ++":"++ show (n + l*(snd x) - 1)
       colExtent = show start ++ ":" ++ show (start + l - 1)
   in "G_("++ rowExtent ++", " ++ colExtent  ++ ") = -speye("++ (show l) ++", "++ (show l) ++");\n" 
     ++ createMatrixG xs ns table

  -- gets the variables in the cone
  coneVar :: SOC -> ([Var],[Int])
  coneVar (SOC vars) = (vars, [coneLength vars])
  coneVar (SOCelem vars) = let n = rows (vars!!0)
    in (vars,(take n (repeat $ length vars)))

  -- gets the size of the cone by looking at the variables' sizes in the cone
  coneLength :: [Var] -> Int
  coneLength x = let varSizes = map (\x -> (rows x)*(cols x)) x
    in foldl (+) 0 varSizes
  
  -- gets the cone width
  coneWidth :: [Int] -> Int
  coneWidth [x] = 1
  coneWidth x = head x
  
  -- flatten a list of tuples
  flattenp :: [([Var],Int)] -> [(Var, Int)]
  flattenp [] = []
  flattenp [x] = zip (fst x) (repeat (snd x))
  flattenp (x:xs) = zip (fst x) (repeat (snd x)) ++ flattenp xs
  
  -- gets the start index from the list of variables, their lengths, and the group sizes
  getStartIdx :: Int->[([Var],Int)]->[Int]
  getStartIdx n [(vars,g)] = map (+n) [1..length vars]
  getStartIdx n ((vars,g):xs) =
     let m = rows (vars!!0) -- assumes variables have same sizes
         fac = case(g) of
           1 -> coneLength vars
           otherwise -> m*g
     in (map (+n) [1..length vars]) ++ getStartIdx (fac+n) xs

