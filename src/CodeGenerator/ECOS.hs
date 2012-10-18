module CodeGenerator.ECOS(codegenECOS, codegenConelp) where
  import CodeGenerator.Common
  
  codegenECOS :: Problem -> Int -> String
  codegenECOS p c = codegen "ecos" p c
  
  codegenConelp :: Problem -> Int -> String
  codegenConelp p c = codegen "conelp" p c
  
  codegen :: String -> Problem -> Int -> String
  codegen solver p c = 
    let vars = getVariableNames p
        varLens = getVariableSizes p
        startIdx = take (length vars) (scanl (+) 1 varLens)  -- indices change for C code
        varTable = zip vars (zip startIdx varLens)
        n = show $ (foldl (+) 0 varLens)
        bsizes = map (fst.getCoeffSize) (vectorB p)
        m = show $ foldl (+) 0 bsizes
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
    -- ++ ["cg_dump_conelpproblem(c_,G_, h_, dims, A_, b_, 'data.h');"]
  
  -- gets the dimensions for cone constraints
  getConeConstraintsForECOS :: Problem -> VarTable -> (Int, String, String)
  getConeConstraintsForECOS p table = 
   let coneInfo = map coneVar (conesK p)
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
  permuteCones :: [Int]->[Int]->[[VarId]]->[([VarId],Int)]
  permuteCones [] _ _ = []
  permuteCones _ [] _ = []
  permuteCones _ _ [] = []
  permuteCones (p:ps) ns xs  =
   let val = (xs!!(p-1), ns!!(p-1))
   in val:permuteCones ps ns xs

  createMatrixG :: [(VarId,Int)] -> [Int] -> VarTable -> String
  createMatrixG [] _ _  = ""
  createMatrixG _ [] _  = ""
  createMatrixG (x:xs) (n:ns) table = 
   let newNames = (flip lookup table) (label$fst x) -- lookup index
       (start, l) = case (newNames) of
         Nothing -> (0,0)
         Just x -> x
       rowExtent = show n ++":"++ show (snd x) ++":"++ show (n + l*(snd x) - 1)
       colExtent = show start ++ ":" ++ show (start + l - 1)
   in "G_("++ rowExtent ++", " ++ colExtent  ++ ") = -speye("++ (show l) ++", "++ (show l) ++");\n" 
     ++ createMatrixG xs ns table

  -- gets the variables in the cone
  coneVar :: SOC -> ([VarId],[Int])
  coneVar (SOC vars) = (vars, [coneLength vars])
  coneVar (SOCelem vars) = let n = rows (vars!!0)
    in (vars,(take n (repeat $ length vars)))

  -- gets the size of the cone by looking at the variables' sizes in the cone
  coneLength :: [VarId] -> Int
  coneLength x = let varSizes = map (\x -> (rows x)*(cols x)) x
    in foldl (+) 0 varSizes
  
  -- gets the cone width
  coneWidth :: [Int] -> Int
  coneWidth [x] = 1
  coneWidth x = head x
  
  -- flatten a list of tuples
  flattenp :: [([VarId],Int)] -> [(VarId, Int)]
  flattenp [x] = zip (fst x) (repeat (snd x))
  flattenp (x:xs) = zip (fst x) (repeat (snd x)) ++ flattenp xs
  
  -- gets the start index from the list of variables, their lengths, and the group sizes
  getStartIdx :: Int->[([VarId],Int)]->[Int]
  getStartIdx n [(vars,g)] = map (+n) [1..length vars]
  getStartIdx n ((vars,g):xs) =
     let m = rows (vars!!0) -- assumes variables have same sizes
         fac = case(g) of
           1 -> coneLength vars
           otherwise -> m*g
     in (map (+n) [1..length vars]) ++ getStartIdx (fac+n) xs

