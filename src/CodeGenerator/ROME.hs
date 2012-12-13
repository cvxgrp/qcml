module CodeGenerator.ROME(codegenROME) where
  import CodeGenerator.Common
  
  codegenROME :: Codegen -> String
  codegenROME prob = 
    let p = problem prob
        vars = getVariableNames p
        varLens = getVariableRows p
        startIdx = init (scanl (+) 1 varLens)  -- indices change for C code
        varTable = zip vars (zip startIdx varLens)
        n = show $ (foldl (+) 0 varLens)
        bsizes = getBRows p
        m = show $ foldl (+) 0 bsizes
        (k, g, cones) = getConeConstraintsForROME p varTable
        nk = show (fromIntegral (length vars) - k)
        kshow = show k
        csign
          | sense p == Minimize = "1"
          | sense p == Maximize = "-1"
          | sense p == Find = "0"
    in unlines $ ["c_ = sparse(" ++ n ++ ",1);",
      "c_(1) = " ++ csign ++ ";",
      "b_ = sparse(" ++ m ++ ",1);",
      getBForCodegen p ++
      "A_ = sparse(" ++ m ++ ", " ++ n ++ ");",
      getAForCodegen p varTable,
      "G_ = sparse("++kshow++", " ++ n ++ ");\n" ++
      g ++
      "h_ = zeros("++ kshow ++ ", 1);",
      cones,
      "cone_.zero = " ++ m ++ ";",
      -- i don't do the pre-solve, so this is the "easy way" out
      "A__ = [A_; G_]; b__ = [b_; h_];",
      "data_.A = A__; data_.b = b__; data_.c = c_;",
      "params_.GEN_PLOTS = false; params_.alpha = 1.8; % alpha is overrelaxation",
      "params_.EPS_ABS=1e-8; params_.EPS_REL=1e-8; params_.MAX_ITERS=10000;",
      "[x_codegen, y_, scoop_status] = rome_cone_solve(data_,cone_,params_);",
      "cvx_begin",
      "  variables xx("++n++") ss("++kshow++" + " ++ m ++")",
      "  minimize (c_'*xx)",
      "  subject to",
      "    A__*xx + ss == b__",
      "    ss(1:cone_.zero) == 0",
      "    ss(cone_.zero+1:cone_.zero+cone_.lp) >= 0",
      "    kk = cone_.zero+cone_.lp;",
      "    for i = 1:cone_.k_soc,",
      "      norm(ss(kk+1:kk+cone_.ns_soc(i)-1)) <= ss(kk+cone_.ns_soc(i))",
      "      kk = kk + cone_.ns_soc(i);",
      "    end",
      "cvx_end",
      "x_codegen = xx;",
      "scoop_status = cvx_status;"

      --"x_codegen(ind) = xtmp;",
      --"x_codegen = x_codegen + g_ - F_*x_codegen;",
      --"ind = (x_codegen == 0); tmp = g_ - F_*x_codegen;",
      --"x_codegen(ind) = tmp(ind);"
    ] ++ socpToProb varTable
    ++ ["scoop_optval = x_codegen(1);"]
    -- dump data to a header file to call ecos c code...
    -- ++ ["cg_dump_conelpproblem(c_,G_, h_, dims, A_, b_, 'data.h');"]
  
  -- gets the dimensions for cone constraints (TODO/XXX: shared with ECOS)
  getConeConstraintsForROME :: SOCP -> VarTable -> (Integer, String, String)
  getConeConstraintsForROME p table = 
   let coneInfo = map coneVar (cones_K p)
       coneVariables = map fst coneInfo
       coneSizes = map snd coneInfo
       -- generate permutation vector (put smallest cones up front)
       coneGroupSizes = zip (map head coneSizes) [1..fromIntegral(length coneSizes)]
       pvec = map (snd) (sort coneGroupSizes)
       -- get the width of the cones (number of cones to generate per var)
       ncones = map coneWidth coneSizes
       -- permuted cone variables
       permuted = permuteCones pvec ncones coneVariables
       startIdx = getStartIdx 0 permuted
       flattenPermuted = flattenp permuted
       matrixG = createMatrixG flattenPermuted startIdx table
       m = cumsum (concat coneSizes)
       higherDimCones = sort $ filter (>1) (concat coneSizes)
       l = m - (cumsum higherDimCones)
   in (m, matrixG, "cone_.k_soc = " ++ show (length higherDimCones) ++ ";\ncone_.ns_soc = " ++ show higherDimCones ++ ";\ncone_.lp = " ++ show l ++ ";")

  -- permute the cones
  permuteCones :: [Integer]->[Integer]->[[Var]]->[([Var],Integer)]
  permuteCones [] _ _ = []
  permuteCones _ [] _ = []
  permuteCones _ _ [] = []
  permuteCones (p:ps) ns xs  =
   let val = (xs!!(fromIntegral p-1), ns!!(fromIntegral p-1))
   in val:permuteCones ps ns xs

  createMatrixG :: [(Var,Integer)] -> [Integer] -> VarTable -> String
  createMatrixG [] _ _  = ""
  createMatrixG _ [] _  = ""
  createMatrixG (x:xs) (n:ns) table = 
   let newNames = (flip lookup table) ((name.fst) x) -- lookup index
       (start, l) = case (newNames) of
         Nothing -> (0,0)
         Just x -> x
       rowExtent = show n ++":"++ show (snd x) ++":"++ show (n + l*(snd x) - 1)
       colExtent = show start ++ ":" ++ show (start + l - 1)
   in "G_("++ rowExtent ++", " ++ colExtent  ++ ") = -speye("++ (show l) ++ ");\n" 
     ++ createMatrixG xs ns table

  -- gets the variables in the cone
  coneVar :: SOC -> ([Var],[Integer])
  coneVar (SOC vars) = let rotatedVars = rotate vars
    in (rotatedVars, [coneLength rotatedVars])
  coneVar (SOCelem vars) = 
    let rotatedVars = rotate vars
        n = fromIntegral $ rows (rotatedVars!!0)
    in (rotatedVars,(take n (repeat $ fromIntegral (length rotatedVars))))

  -- gets the size of the cone by looking at the variables' sizes in the cone
  coneLength :: [Var] -> Integer
  coneLength x = let varSizes = map (\x -> (rows x)*(cols x)) x
    in foldl (+) 0 varSizes
  
  -- gets the cone width
  coneWidth :: [Integer] -> Integer
  coneWidth [x] = 1
  coneWidth x = head x
  
  -- flatten a list of tuples
  flattenp :: [([Var],Integer)] -> [(Var, Integer)]
  flattenp [] = []
  flattenp [x] = zip (fst x) (repeat (snd x))
  flattenp (x:xs) = zip (fst x) (repeat (snd x)) ++ flattenp xs

  rotate :: [a] -> [a]
  rotate [] = []
  rotate [x] = [x]
  rotate (x:xs) = xs ++ [x]
  
  -- gets the start index from the list of variables, their lengths, and the group sizes
  getStartIdx :: Integer->[([Var],Integer)]->[Integer]
  getStartIdx n [(vars,g)] = 
    let ms = case(g) of
          1 -> init $ scanl (+) 1 (map rows vars)
          otherwise -> [1..fromIntegral (length vars)]
    in map (+n) ms
  getStartIdx n ((vars,g):xs) =
     let m = rows (vars!!0) -- assumes variables have same sizes
         fac = case(g) of
           1 -> coneLength vars
           otherwise -> m*g
         ms = case(g) of
           1 -> init $ scanl (+) 1 (map rows vars)
           otherwise -> [1..fromIntegral (length vars)]
     in (map (+n) ms) ++ getStartIdx (fac+n) xs

 -- TODO/XXX: writing code generators is not as easy as it seems, especially when SOC have diff conventions
 -- e.g. (u,v) \in SOC => ||u|| <= v OR ||v|| <= u .... 
