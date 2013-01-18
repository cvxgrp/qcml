{--

Copyright (c) 2012-2013, Eric Chu (eytchu@gmail.com)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met: 

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. 
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are
those of the authors and should not be interpreted as representing official
policies, either expressed or implied, of the FreeBSD Project.

--}

module CodeGenerator.ECOS(codegenECOS, codegenConelp) where
  import CodeGenerator.Common
  
  codegenECOS :: Codegen -> String
  codegenECOS p = codegen "ecos" (problem p)
  
  codegenConelp :: Codegen -> String
  codegenConelp p = codegen "conelp" (problem p)
  
  codegen :: String -> SOCP -> String
  codegen solver p = 
    let vars = getVariableNames p
        varLens = getVariableRows p
        startIdx = init (scanl (+) 1 varLens)  -- indices change for C code
        varTable = zip vars (zip startIdx varLens)
        n = show $ (foldl (+) 0 varLens)
        bsizes = getBRows p
        m = show $ foldl (+) 0 bsizes
        (k, g, cones) = getConeConstraintsForECOS p varTable
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
  getConeConstraintsForECOS :: SOCP -> VarTable -> (Integer, String, String)
  getConeConstraintsForECOS p table = 
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
   in (m, matrixG, "dims.q = " ++ show higherDimCones ++ ";\ndims.l = " ++ show l ++ ";")

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
   in "G_("++ rowExtent ++", " ++ colExtent  ++ ") = -speye("++ (show l) ++", "++ (show l) ++");\n" 
     ++ createMatrixG xs ns table

  -- gets the variables in the cone
  coneVar :: SOC -> ([Var],[Integer])
  coneVar (SOC vars) = (vars, [coneLength vars])
  coneVar (SOCelem vars) = let n = fromIntegral $ rows (vars!!0)
    in (vars,(take n (repeat $ fromIntegral (length vars))))

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
  
  -- gets the start index from the list of variables, their lengths, and the group sizes
  getStartIdx :: Integer->[([Var],Integer)]->[Integer]
  getStartIdx n [(vars,g)] = map (+n) [1..fromIntegral (length vars)]
  getStartIdx n ((vars,g):xs) =
     let m = rows (vars!!0) -- assumes variables have same sizes
         fac = case(g) of
           1 -> coneLength vars
           otherwise -> m*g
     in (map (+n) [1..fromIntegral (length vars)]) ++ getStartIdx (fac+n) xs

