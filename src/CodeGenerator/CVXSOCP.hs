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

module CodeGenerator.CVXSOCP (codegen) where
  import CodeGenerator.Common
  
  {-- what follows is for codegen --}
  codegen :: Codegen -> String
  codegen x = let p = problem x
                  vars = getVariableNames p
                  varLens = getVariableRows p
                  startIdx = take (length vars) (scanl (+) 1 varLens)  -- indices change for C code
                  varTable = zip vars (zip startIdx varLens)
                  n = show $ (foldl (+) 0 varLens)
                  bsizes = getBRows p
                  m = show $ foldl (+) 0 bsizes
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
      "cvx_begin",
      "variable x_codegen(" ++ n ++ ")",
      "minimize (c_'*x_codegen)",
      "A_*x_codegen == b_"
    ] ++ getConeConstraintsForCodegen p varTable ++
    ["cvx_end"] ++ socpToProb varTable
    ++ ["ecos_optval = "++csign++"*cvx_optval;"]

  -- gets the cone constraints
  getConeConstraintsForCodegen :: SOCP -> VarTable ->[String]
  getConeConstraintsForCodegen p table = map (convertConeForCodegen table) (cones_K p)
  
  -- converts cone constraints to CVX string
  convertConeForCodegen :: VarTable -> SOC -> String
  convertConeForCodegen table (SOC vars) = 
   let newNames = map (flip lookup table) (map name vars)
   in case (length vars) of
     1 -> (extractString $ head newNames) ++ " >= 0"
     otherwise -> "norm([" ++ (intercalate ", " (map extractString (tail newNames))) ++ "]) <= " ++ (extractString $ head newNames)
  convertConeForCodegen table (SOCelem vars) = 
    let newNames = map (flip lookup table) (map name vars)
    in case (length vars) of
      1 -> (extractString $ head newNames) ++ " >= 0"
      otherwise -> "norms([" ++ (intercalate ", " (map extractString (tail newNames))) ++ "],[],2) <= " ++ (extractString $ head newNames)

  extractString :: Maybe (Integer,Integer) -> String
  extractString Nothing = ""
  extractString (Just (x,l)) = "x_codegen(" ++ show x ++ ":" ++ show (x+l-1) ++ ")"