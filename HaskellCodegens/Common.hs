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

module CodeGenerator.Common (
  getVariableNames, 
  getVariableRows,
  getBRows,
  getVariableInfo,
  socpToProb, 
  getAForCodegen,
  getBForCodegen,
  VarTable,
  Codegen(..),
  getCoeffInfo,
  getCoeffSize,
  coeffRows,
  cones_K, affine_A, affine_b,
  cumsum,
  module Expression.SOCP,
  module Data.List) where

  import Expression.SOCP
  import Expression.Expression(Expr)
  import Data.List
  import qualified Data.Map as M


  data Codegen = Codegen {
    problem :: SOCP,
    symbolTable :: M.Map String Expr
  }

  -- XXX/TODO: code generator is long overdue for a rewrite

  -- helper functions
  cones_K = conesK.constraints
  affine_A = matrixA.constraints
  affine_b = vectorB.constraints

  cumsum :: Num a => [a] -> a
  cumsum = foldl' (+) 0

  -- a VarTable is an associatiation list with (name, (start, len))
  type VarTable = [(String, (Integer,Integer))]
  
  getVariableNames :: SOCP -> [String]
  getVariableNames p = map name (getVariableInfo p)
  
  getVariableRows :: SOCP -> [Integer]
  getVariableRows p = map rows (getVariableInfo p)

  getBRows :: SOCP -> [Integer]
  getBRows p = map coeffRows (affine_b p) 
  
  -- gets the list of unique variable names for CVX
  -- starts with cone vars
  -- objective var is at the end
  getVariableInfo :: SOCP -> [Var]
  getVariableInfo p = 
    let objectiveVar = obj p
        aVars =  concat $ map variables (affine_A p)  -- gets the list of all variables in the affine constraints
        coneVars = concat $ map variables (cones_K p) -- get the list of all variables in the cones_K
        allVariables = [objectiveVar] ++ coneVars ++ aVars
        uniqueVarNames = nubBy (\x y-> name x == name y) allVariables
    in uniqueVarNames
    
  -- write out results
  socpToProb :: VarTable -> [String]
  socpToProb table = map (\(s,(i,l)) -> s ++ " = x_codegen(" ++ show i ++ ":" ++ show (i+l-1) ++ ");") table
  
  -- get A
  getAForCodegen = getAForCodegenWithIndx 1
  getAForCodegenC = getAForCodegenWithIndx 0

  getAForCodegenWithIndx :: Integer -> SOCP -> VarTable -> String
  getAForCodegenWithIndx i p table = 
    let bsizes = map coeffRows (affine_b p)  -- height of each row
        startIdx = take (length bsizes) (scanl (+) i bsizes)  -- gives start index for each row
    in intercalate "\n" (map (createRow table) (zip (affine_A p) startIdx))
  
  createRow :: VarTable -> (Row,Integer) -> String
  createRow table (row,ind) = 
    let indices = map (flip lookup table) (varnames row)
        coefficients = coeffs row
        -- with our setup, the only time rowHeights aren't equal to the first one is when we are concatenating
        rowHeights = map coeffRows (tail coefficients)
        rowTotal = coeffRows (head coefficients)
        offsets
          | all (==rowTotal) rowHeights = 0:(map (rowTotal-) rowHeights)
          | otherwise = 0:rowHeights
        shifts = init $ scanl (+) 0 offsets
    in intercalate " " (zipWith (assignToA ind) shifts (zip (elems row) indices))
    
  -- get coeff size and value
  getCoeffInfo :: Coeff -> (Integer,Integer,String)
  getCoeffInfo (Matrix p) = (rows p, cols p, name p)
  getCoeffInfo (MatrixT p) = (cols p, rows p, (name p) ++ "'")
  getCoeffInfo (Vector n p) = (n,1, name p)
  getCoeffInfo (VectorT n p) = (n,1, (name p)++"'")
  getCoeffInfo (Diag 1 p) = (1,1, name p)
  getCoeffInfo (Diag n p) = (n,n,"spdiags("++ name p ++ ",0,"++show n++","++show n++")")
  getCoeffInfo (OnesT n 0) = (1,n, "0")
  getCoeffInfo (OnesT 1 x) = (1,1, show x)
  getCoeffInfo (OnesT n x) = (1,n, (show x)++"*ones(1, "++show n ++")")
  getCoeffInfo (Ones n 0) = (n,1, "0")
  getCoeffInfo (Ones 1 x) = (1,1, show x)
  getCoeffInfo (Ones n x) = (n,1, (show x)++"*ones("++show n ++", 1)")
  getCoeffInfo (Eye n 0) = (n,n,"0")
  getCoeffInfo (Eye 1 x) = (1,1, show x)
  getCoeffInfo (Eye n x) = (n,n,(show x) ++ "*speye("++show n++", "++show n++")")
  
  -- just get coeff size
  getCoeffSize :: Coeff -> (Integer, Integer)
  getCoeffSize x = let (m,n,s) = getCoeffInfo x
    in (m,n)
    
  -- just get coeff rows
  coeffRows :: Coeff -> Integer
  coeffRows x = let (m,n,s) = getCoeffInfo x
    in m
  
  -- XXX: this is such a bizarre function type signature...
  assignToA :: Integer -> Integer -> ((Coeff, Var), Maybe (Integer,Integer)) -> String
  assignToA _ _ (_, Nothing) = ""
  assignToA x offset (row, Just (y,l)) = 
    let (m,n,val) = getCoeffInfo (fst row) -- n should equal l at this point!!
        rowExtent = show (x+offset) ++ ":" ++ show (x+offset+m-1)
        colExtent = show y ++ ":" ++ show (y+n-1)
    in case(val) of
      "0" -> ""
      otherwise -> "A_(" ++ rowExtent ++ ", " ++ colExtent ++ ") = " ++ val ++ ";"
  
  -- get b
  getBForCodegen = getBForCodegenWithIndx 1
  getBForCodegenC = getBForCodegenWithIndx 0

  getBForCodegenWithIndx :: Integer -> SOCP -> String
  getBForCodegenWithIndx i p = 
    let b = affine_b p
        sizes = map coeffRows b
        startIdx = init $ scanl (+) i sizes -- start index changes for C code
    in concat $ map assignToB (zip b startIdx)
  
  assignToB :: (Coeff,Integer) -> String
  assignToB (val, ind) = 
    let (m,n,s) = getCoeffInfo val
    in case (s) of
      "0" -> ""
      otherwise -> "b_("++ (show ind) ++ ":"++ show (ind+m-1)++") = " ++ s ++ ";\n"