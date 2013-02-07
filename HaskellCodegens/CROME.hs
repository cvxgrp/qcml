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

module CodeGenerator.CROME(cRomeHeader, cRomeCodegen, cRomeTestSolver, romeMakefile, romeParamlist, romeVarlist, romeParamsigns) where
  import CodeGenerator.Common
  import Expression.Expression
  import qualified Data.Map as M

  -- need this for random numbers
  -- import System.Random

  import Data.Maybe
  -- TODO/XXX: if we know the target architecture, we can make further optimizations such
  -- as memory alignment. but as it stands, we're just generating flat C
  --
  -- also, if we knew the target, we could emit things like memset or memcpy, which can be
  -- significantly faster for the target platform. unfortunately, because memset and memcpy
  -- are dependent on the target architecture, a "for" loop is the most portable solution
  --
  -- also, i'm sure the memcpy / memmove / memset operations are not the bottleneck

  -- TODO/XXX: need to figure out what Alex's "minimum" package for ECOS is so it can be
  -- distributed alongside the generated code

  -- TODO/XXX: annotate sparsity structure (assume it's known at "compile" time)

  -- built on top of ECOS
  cRomeHeader :: String -> String -> Codegen -> String
  cRomeHeader ver desc x = unlines $
    ["/* stuff about open source license",
     " * ....",
     " * The problem specification for this solver is: ",
     " *", 
     probDesc desc,
     " *",
     " * For now, parameters are *dense*, and we don't respect sparsity. The sparsity",
     " * structure has to be specified during code generation time. We could do the more",
     " * generic thing and allow sparse matrices, but as a first cut, we won't.",
     " * Version " ++ ver,
     " * Eric Chu, Alex Domahidi, Neal Parikh, Stephen Boyd (c) 2012 or something...",
     " */",
     "",
     "#ifndef __SOLVER_H__",
     "#define __SOLVER_H__",
     "",
     "#include \"coneOS.h\"",
     ""]
     ++ paramCode
     ++ varCode ++ [
     "void setup(const params *p, Data **d, Cone **k);        /* setting up workspace (assumes params already declared) */",
     "void solve(Data *d, Cone *k, vars *sol); /* solve the problem (assumes vars already declared) */",
     "void cleanup(Data *d, Cone *k);         /* clean up workspace */",
     "",
     "#endif    /* solver.h */"]
    where 
      params = romeParamlist x
      paramCode = [paramStruct (romeParamlist x)]
      varCode = [variableStruct (romeVarlist x)]
      --arglist = filter (/="") (map toArgs params)

  cRomeCodegen :: Codegen -> String
  cRomeCodegen x = unlines
    ["#include \"solver.h\"", "",
     setupFunc x,
     solverFunc x,
     cleanupFunc]  -- cleanup function is inlined

  -- although we'd like to generate "branch-free" code, for large-ish problems, this is not going to work
  cRomeTestSolver :: Codegen -> String
  cRomeTestSolver x = unlines $
    ["#include \"solver.h\"",
     "#include <stdlib.h> /* for random numbers */",
     "#include <time.h> /* for seed */",
     "",
     "double randu()",
     "{",
     "  return ((double) rand())/RAND_MAX;",
     "}",
     "",
     "int main(int argc, char **argv)",
     "{",
     "  srand( time(NULL) );",
     "  int i = 0;",
     "  int j = 0;",
     "  params p;",
     paramStrings,
     "  Cone *k = NULL; Data *d=NULL; setup(&p, &d, &k);",
     "  /* int flag = 0; */",
     "  vars v;",
     "  solve(d, k, &v);",
     "  cleanup(d, k);",
     "  return 1;",
     "}"]
     where params = romeParamlist x
           paramSizes = map ((\(x,y) -> x*y).dimensions) params
           n = cumsum paramSizes
           paramStrings = intercalate "\n" $ map expandParam params
           -- randVals = take n (randoms (mkStdGen seed) :: [Double])  -- TODO/XXX: doesn't take param signs in to account yet
           -- paraminits = intercalate "\n" [ s ++ " = " ++ (show v) ++ ";"  | (s,v) <- zip paramStrings randVals]

  romeMakefile :: String -> Codegen -> String
  romeMakefile coneos_path _ = unlines $
    [ "CONEOS_PATH = " ++ coneos_path,
      "INCLUDES = -I$(CONEOS_PATH)/code",
      "# uses direct solver",
      "DIRLIBS = $(CONEOS_PATH)/code/libconeOSdir.a $(CONEOS_PATH)/code/direct/external/AMD/Lib/libamd.a $(CONEOS_PATH)/code/direct/external/LDL/Lib/libldl.a",
      "# uses indirect solver",
      "INDIRLIBS = $(CONEOS_PATH)/code/libconeOSindir.a",
      "LIBS += -lm",
      "",
      ".PHONY: all",
      "all: testsolver_indir testsolver_dir",
      "",
      "testsolver_dir: solver.o",
      "\tgcc -Wall -O3 -o testsolver_dir testsolver.c solver.o $(DIRLIBS) $(INCLUDES)",
      "",
      "testsolver_indir: solver.o",
      "\tgcc -Wall -O3 -fopenmp -o testsolver_indir testsolver.c solver.o $(INDIRLIBS) $(INCLUDES)",
      "",
      "solver.o: solver.c",
      "\tgcc -std=c99 -Wall -O3 -c solver.c $(INCLUDES)",
      "",
      "clean:",
      "\trm testsolver_dir testsolver_indir *.o"]

  -- helper functions

  probDesc :: String -> String
  probDesc desc = intercalate ("\n") (map (" *     "++) (lines desc))

  -- XXX/TODO: romeVarlist and romeParamlist may be special.... (may need to export them)
  romeVarlist :: Codegen -> [Var]
  romeVarlist c = catMaybes maybe_vars
    where maybe_vars = map (extractVar) (M.elems $ symbolTable c)

  romeParamlist :: Codegen -> [Param]
  romeParamlist c = catMaybes maybe_params
    where maybe_params = map (extractParam) (M.elems $ symbolTable c)

  romeParamsigns :: Codegen -> [Sign]
  romeParamsigns c = catMaybes maybe_signs
    where maybe_signs = map (extractParamSigns) (M.elems $ symbolTable c)

  extractVar :: Expr -> Maybe Var
  extractVar (Variable v) = Just v
  extractVar _ = Nothing

  extractParam :: Expr -> Maybe Param
  extractParam (Parameter v _ _) = Just v
  extractParam _ = Nothing

  extractParamSigns :: Expr -> Maybe Sign
  extractParamSigns (Parameter _ s _) = Just s
  extractParamSigns _ = Nothing

  -- structs for header file

  variableStruct :: [Var] -> String
  variableStruct variables = unlines $ 
    ["/*",
     " * struct vars_t (or `vars`)",
     " * =========================",
     " * This structure stores the solution variables for your problem.",
     " *",
     " */",
     "typedef struct vars_t {"]
     ++ map toVarString variables ++
     [
    -- "  double *dualvars;", -- TODO: dual vars
     "} vars;"]

  toVarString :: Var -> String
  toVarString (Var s (1,1)) = "  double " ++ s ++ ";"
  toVarString (Var s (m,1)) = "  double " ++ s ++ "[" ++ show m ++ "];"
  toVarString _ = "  double error;" -- should never run in to this case.. but.. what if?
 
  paramStruct :: [Param] -> String
  paramStruct params = unlines $
    ["/*",
     " * struct params_t (or `params`)",
     " * =============================",
     " * This structure contains the data for all parameters in your problem.",
     " *",
     " */",
     "typedef struct params_t {"]
     ++ map toParamString params ++
     ["} params;"]

  toParamString :: Param -> String
  toParamString (Param s (1,1)) = "  double " ++ s ++ ";"
  toParamString (Param s (m,1)) = "  double " ++ s ++ "[" ++ show m ++ "];"
  -- toParamString (Param s (1,m) _) = "  double " ++ s ++ "[" ++ show m ++ "];"
  toParamString (Param s (m,n)) = "  double " ++ s ++ "[" ++ show m ++ "]["++ show n ++ "];"

  expandParam :: Param -> String
  expandParam (Param s (1,1)) = "  p." ++ s ++ " = randu();"
  expandParam (Param s (m,1)) = intercalate "\n" [
    "  for(i = 0; i < " ++ show m ++ "; ++i) {",
    "    p." ++ s ++ "[i] = randu();",
    "  }"] 
  -- expandParam (Param s (1,m) _) = "  double " ++ s ++ "[" ++ show m ++ "];"
  expandParam (Param s (m,n)) = intercalate "\n" [
    "  for(i = 0; i < " ++ show m ++ "; ++i) {",
    "    for(j = 0; j < " ++ show n ++ "; ++j) {",
    "      p." ++ s ++ "[i][j] = randu();",
    "    }",
    "  }"] 


  -- functions to generate functions for c source

  buildVarTable :: Codegen -> VarTable
  buildVarTable x = varTable
    where p = problem x
          vars = getVariableNames p
          varLens = getVariableRows p
          startIdx = init (scanl (+) 0 varLens)  -- indices change for C code
          varTable = zip vars (zip startIdx varLens)

  solverFunc :: Codegen -> String
  solverFunc x = unlines $
     ["void solve(Data *d, Cone *k, vars *sol)",
     "{",
     "  int i = 0;",
     "  Sol *coneOSsol = coneOS(d,k);"]
     ++ zipWith expandVarIndices vars varInfo
     ++ ["  free(coneOSsol->x);",
         "  free(coneOSsol->z);",
         "  free(coneOSsol->status);",
         "  free(coneOSsol);",
         "}"]
     -- ++["  return exitflag;", "}"]
    where vars = romeVarlist x -- variables in the problems
          varNames = map name vars
          varTable = buildVarTable x -- all variables introduced in problem rewriting
          varInfo = catMaybes $ map (flip lookup varTable) varNames

  expandVarIndices :: Var -> (Integer, Integer) -> String
  expandVarIndices v (ind, 1) = "  sol->" ++ (name v) ++ " = coneOSsol->x["++ show ind ++"];"
  expandVarIndices v (ind, _) = intercalate "\n" [
    "  for(i = 0; i < " ++ show (rows v) ++ "; ++i) {",
    "    sol->"++ (name v) ++ "[i] = coneOSsol->x[i + " ++ show ind ++ "];",
    "  }"] 
    --"  sol->" ++ (name v) ++ "["++ show i ++"] = w->x["++ show (ind + i) ++"];" | i <- [0..(rows v - 1)]]

    --"  memcpy(sol->" ++ (name v) ++ ", w->x + " ++ show ind ++ ", sizeof(double)*" ++ (show $ rows v) ++ ");"


  setupFunc :: Codegen -> String
  setupFunc x = unlines $ 
    ["void setup(const params *p, Data **d, Cone **k)",
     "{",
     "  int i = 0;",
     "  double *ptr;",
     "  *d = malloc(sizeof(Data));",
     "  *k = malloc(sizeof(Cone));",
     setBval p k,
     "  (*d)->m =" ++ show (m+k) ++ ";",
     "  (*d)->n =" ++ show n ++ ";",
     "  (*d)->b = b;",
     "  static double c[" ++ show n ++ "] = {" ++ setC p ++ "}; /* rest = {0.0}; */",
     "  (*d)->c = c;",
     -- "  static double h[" ++ show k ++ "]; /* = {0.0}; */",
     setQ higherDimCones,
     "  (*k)->q = q;",
     -- setG varTable p,
     setA varTable p,
     "  (*d)->Ax = Apr;",
     "  (*d)->Ai = Air;",
     "  (*d)->Ap = Ajc;",
     "  (*k)->f = " ++ show m ++ ";",
     "  (*k)->l = " ++ show l ++ ";",
     "  (*k)->qsize = " ++ show (length higherDimCones) ++ ";",
     "}"]
    where 
      p = problem x
      params = romeParamlist x
      varLens = getVariableRows p
      bLens = getBRows p
      coneLens = coneSizes p
      higherDimCones = sort $ filter (>1) coneLens
      m = cumsum bLens
      n = cumsum varLens
      k = cumsum coneLens
      l = k - (cumsum higherDimCones)
      arglist = intercalate ", " 
        [show n ++ " /* num vars */", 
         show k ++ " /* num cone constraints */", 
         show m ++ " /* num eq constraints */", 
         show l ++ " /* num linear cones */", 
         show (length higherDimCones) ++ " /* num second-order cones */"]
      varTable = buildVarTable x -- all variables introduced in problem rewriting


  setC :: SOCP -> String
  setC p = case (sense p) of
      Minimize -> "1"
      Maximize -> "-1"
      Find -> "0"


  setBval :: SOCP -> Integer -> String
  setBval p k = intercalate "\n" $
      ["  static double b[" ++ show (m+k) ++ "] = {" ++ expandBConst bCoeffs ++ "}; /* rest = 0.0 */"] ++
      catMaybes (zipWith expandBCoeff bCoeffs startIdxs)
    where bCoeffs = affine_b p
          bLens = getBRows p
          m = last startIdxs
          startIdxs = scanl (+) 0 bLens

  expandBConst :: [Coeff] -> String
  expandBConst xs = intercalate ", " (map showCoeff xs)

  -- only shows value of constants (for initialization list)
  showCoeff :: Coeff -> String
  showCoeff (Ones n x) = intercalate ", " $ (take (fromIntegral n) (repeat (show x)))
  showCoeff (Vector n p) = intercalate ", " $ (take (fromIntegral n) (repeat "0"))
  showCoeff (VectorT n p) = intercalate ", " $ (take (fromIntegral n) (repeat "0"))
  showCoeff (OnesT n x) = intercalate ", " $ (take (fromIntegral n) (repeat (show x)))
  showCoeff _ = ""

  -- only expands non constants in to loops
  expandBCoeff :: Coeff -> Integer -> Maybe String
  expandBCoeff (Vector 1 p) idx = Just $ "  b[" ++ show idx ++ "] = p->" ++ (name p) ++ ";"
  expandBCoeff (Vector n p) idx = Just $ intercalate "\n" [
    "  ptr = (double *) p->" ++ (name p) ++ ";",
    "  for(i = " ++ show idx ++ "; i < " ++ show (idx+n) ++ "; ++i) {",
    "    b[i] = *ptr++;",
    "  }"]
  expandBCoeff (VectorT 1 p) idx = Just $ "  b[" ++ show idx ++ "] = p->" ++ (name p) ++ ";"
  expandBCoeff (VectorT n p) idx = Just $ intercalate "\n" [
    "  ptr = (double *) p->" ++ (name p) ++ ";",
    "  for(i = " ++ show idx ++ "; i < " ++ show (idx+n) ++ "; ++i) {",
    "    b[i] = *ptr++;",
    "  }"]
  --"  b[" ++ show (idx + i) ++ "] = p->" ++ (name p) ++ "[" ++ show i ++ "];" | i <- [0 .. (n - 1)]]
  expandBCoeff _ _ = Nothing

  cleanupFunc :: String
  cleanupFunc = unlines $ [
    "void cleanup(Data *d, Cone *k)",
    "{",
    "  free(d);free(k);",
    "}"]

  setQ :: [Integer] -> String
  setQ xs = "  static int q[" ++ show q ++ "] = {" ++ vals ++ "};"
    where vals = intercalate ", " (map show xs)
          q = length xs

  -- setG :: VarTable -> SOCP -> String
  -- setG table p = intercalate "\n" $
  --   ["  static int Gjc[" ++ show (n+1) ++ "] = {" ++ jc ++ "};",
  --    "  static int Gir[" ++ show nnz ++ "] = {" ++ ir ++ "};",
  --    "  static double Gpr[" ++ show nnz ++ "] = {" ++ pr ++ "};"]
  --   where varLens = getVariableRows p
  --         n = cumsum varLens
  --         cones = cones_K p
  --         coneGroupSizes = map coneGroups cones
  --         -- generate permutation vector for cone groups (put smallest cones up front)
  --         forSortingCones = zip (map head coneGroupSizes) [1..fromIntegral (length coneGroupSizes)]
  --         pvec = map snd (sort forSortingCones)  -- permutation vector
  --         -- sort the cones and get the new sizes
  --         sortedCones = reorderCones pvec cones
  --         sortedSizes = map coneGroups sortedCones
  --         startIdxs = scanl (+) 0 (map cumsum sortedSizes)
  --         -- matrix G
  --         gmat = concat (zipWith (createCone table) sortedCones startIdxs) 
  --         -- G in (i,j,-1) form
  --         matrixG = sortBy columnsOrder' gmat -- G in (i,j,-1) form, but sorted according to columns
  --         -- for printing
  --         nnz = length matrixG
  --         nnzPerRow = countNNZ' n 0 0 matrixG
  --         jc = intercalate ", " (map show (scanl (+) 0 nnzPerRow))
  --         ir = intercalate ", " (map (show.fst) matrixG)
  --         pr = intercalate ", " (take nnz $ repeat "-1.0")

  -- gets the list of cone sizes
  coneSizes :: SOCP -> [Integer]
  coneSizes p = concat (map coneGroups (cones_K p))

  -- gets the cones associated with each variable, coneGroups (SOCelem [x,y,z]) = take (rows x) [3,...] 
  coneGroups :: SOC -> [Integer]
  coneGroups (SOC vars) = [cumsum (map (\x -> (rows x)*(cols x)) vars)]
  coneGroups (SOCelem vars) = (take (fromIntegral $ rows (vars!!0)) (repeat $ fromIntegral (length vars)))

  --coneVars :: SOCP -> [Var]
  --coneVars p = concat (map vars (cones_K p))

  -- reorder the list of cones so that smallest ones are up front
  reorderCones :: [Integer]->[SOC]->[SOC]
  reorderCones [] _ = []
  reorderCones _ [] = []
  reorderCones (p:ps) cones = val : reorderCones ps cones
    where val = cones!!(fromIntegral p - 1)

  -- produces (i,j,val) of nonzero locations in matrix G
  -- third argument "idx" is *row* start index
  createCone :: VarTable -> SOC -> Integer -> [(Integer,Integer)]
  createCone table (SOC vars) idx = concat [expandCone i k | (i,k) <- zip idxs sizes ]
    where idxs = scanl (+) idx (map rows vars)
          sizes = catMaybes $ map (flip lookup table) (map name vars)
  createCone table (SOCelem vars) idx = concat [expandConeElem idx n i j k | (i,k) <- zip [0,1 ..] sizes, j <- [0 .. (m-1)]]
    where n = fromIntegral (length vars)
          m = rows (vars!!0)
          sizes = catMaybes $ map (flip lookup table) (map name vars)

  -- SOCElem [x,y,z]
  -- assuming x is a 2 vec starting at ind 10, y is a 2 vec starting at ind 3, z is a 2 vec starting at ind 5
  -- the above should produce a string like
  -- G(0,10) = -1
  -- G(1,3) = -1
  -- G(2,5) = -1
  -- G(3,11) = -1
  -- G(4,4) = -1
  -- G(5,6) = -1
  -- G(6,12) = -1
  expandConeElem :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer) -> [(Integer,Integer)]
  expandConeElem idx n i j (k,l) = [(idx + i + j*n, k + j)] -- "G(" ++ show (idx + i + j*n) ++ ", " ++ show (k + j) ++ ") = -1;"

  expandCone :: Integer -> (Integer, Integer) -> [(Integer,Integer)]
  expandCone idx (m,n) = [(idx +i, m+i) | i <- [0 .. (n-1)]] --intercalate "\n" ["G(" ++ show (idx + i) ++ ", " ++ show (m + i) ++ ") = -1" | i <- [0.. (n-1)]]


  setA :: VarTable -> SOCP -> String
  setA table p = compress n matrixA
    where varLens = getVariableRows p
          n = cumsum varLens
          a = affine_A p
          startIdxs = scanl (+) 0 (map rowHeight a)
          amat = concat (zipWith (createA table) a startIdxs)
          -- the rest of this is to set the matrix G
          cones = cones_K p
          coneGroupSizes = map coneGroups cones
          -- generate permutation vector for cone groups (put smallest cones up front)
          forSortingCones = zip (map head coneGroupSizes) [1..fromIntegral (length coneGroupSizes)]
          pvec = map snd (sort forSortingCones)  -- permutation vector
          -- sort the cones and get the new sizes
          sortedCones = reorderCones pvec cones
          sortedSizes = map coneGroups sortedCones
          gStartIdxs = scanl (+) (last startIdxs) (map cumsum sortedSizes)
          -- matrix G
          gmat = concat (zipWith (createCone table) sortedCones gStartIdxs) 
          -- G in (i,j) form
          gmatWithValue = map (\(x,y) -> (x,y, Const "-1" 0 0 1 1)) gmat
          -- G in (i,j,-1) form
          matrixA = sortBy columnsOrder (amat ++ gmatWithValue)
          
          

  rowHeight :: Row -> Integer
  rowHeight r = maximum (map coeffRows (coeffs r))

  createA :: VarTable -> Row -> Integer -> [(Integer,Integer,Value)]
  createA table row idx = concat [ expandARow i c s | (i,c,s) <- zip3 idxs coefficients sizes ]
    where vars = variables row
          coefficients = coeffs row
          sizes = catMaybes $ map (flip lookup table) (map name vars)
          idxs = getRowStartIdxs idx coefficients   -- only for concatenation

  getRowStartIdxs :: Integer -> [Coeff] -> [Integer]
  getRowStartIdxs idx (c:cs)
    | all (==maxHeight) rowHeights = repeat idx
    | otherwise = idx:(scanl (+) idx rowHeights)   -- this currently works only because the maximum is guaranteed to be the *first* element
    where rowHeights = map coeffRows cs
          maxHeight = coeffRows c


  -- helper functions for generating CCS
  data Value = Value {
      value :: String,
      ri :: Integer,
      cj :: Integer,
      width :: Integer,
      height :: Integer
    } 
    | Const {
      value :: String,
      ri :: Integer,
      cj ::Integer,
      width :: Integer,
      height :: Integer
    } deriving (Eq)

  instance Show Value where
    show x = (value x) ++ "[" ++ show (ri x) ++ "][" ++ show (cj x) ++ "]" 

  expandARow :: Integer -> Coeff -> (Integer, Integer) -> [(Integer,Integer,Value)]
  -- size of coeff should match
  expandARow idx (Eye _ x) (m,n) = [(idx + i, m + i, Const (show x) 0 0 1 1) | i <- [0 .. (n-1)]] -- eye length should equal m
  expandARow idx (Ones n x) (m,1) = [(idx + i, m, Const (show x) 0 0 1 1)| i <- [0 .. (n-1)]]  -- different pattern based on different coeff...
  expandARow idx (OnesT _ x) (m,n) = [(idx, m + i, Const (show x) 0 0 1 1) | i <- [0 .. (n-1)]] -- onesT length should equal m
  expandARow idx (Diag n p) (m,_) = [(idx + i, m + i, toParamVal i 0 p) | i <- [0 .. (n-1)]] -- onesT length should equal m
  expandARow idx (Matrix p) (m,n) = [(idx + i, m + j, toParamVal i j p) | i <- [0 .. (rows p-1)], j <- [0 .. (cols p-1)]]
  expandARow idx (MatrixT p) (m,n) = [(idx + j, m + i, toParamVal i j p) | i <- [0 .. (rows p-1)], j <- [0 .. (cols p-1)]]
  expandARow idx (Vector n p) (m,1) = [(idx + i, m, toParamVal i 0 p) | i <- [0 .. (n-1)]]
  expandARow idx (VectorT _ p) (m,n) = [(idx, m + i, toParamVal i 0 p) | i <- [0 .. (n-1)]]

  toParamVal :: Integer -> Integer -> Param -> Value
  toParamVal i j p = Value (name p) i j (rows p) (cols p)


  -- sorting function for CCS
  columnsOrder' :: (Integer,Integer) -> (Integer,Integer) -> Ordering
  columnsOrder' (m1,n1) (m2,n2)  | n1 > n2 = GT
                                 | n1 == n2 && (m1 > m2) = GT
                                 | otherwise = LT

  columnsOrder :: (Integer,Integer,Value) -> (Integer,Integer,Value) -> Ordering
  columnsOrder (m1,n1,_) (m2,n2,_)  | n1 > n2 = GT
                                    | n1 == n2 && (m1 > m2) = GT
                                    | otherwise = LT

  -- compress (i,j,val) form in to column compressed form and outputs the string
  -- assumes (i,j,val) are sorted by columns
  compress :: Integer -> [(Integer,Integer,Value)] -> String
  compress cols xs = intercalate "\n" $
    ["  static int Ajc[" ++ show (cols+1) ++ "] = {" ++ jc ++ "};",
     "  static int Air[" ++ show nnz ++ "] = {" ++ ir ++ "};",
     "  static double Apr[" ++ show nnz ++ "] = {" ++ pr ++ "};",
     indexMaps] --pvals]
    where nnz = length xs
          nnzPerRow = countNNZ cols 0 0 xs
          jc = intercalate ", " (map show (scanl (+) 0 nnzPerRow))
          ir = intercalate ", " (map (show.getCCSRow) xs)
          indexedValue = zipWith (\(_,_,v) i -> (i,v)) xs [0..]
          aEntries = sortBy valueOrder indexedValue
          (constants, vals) = partition isConst aEntries
          constantsSorted = sortBy indexOrder constants
          pr = intercalate "," (createPrList 0 constantsSorted)
          indexMaps = createIndexMaps vals
          -- rowIdxs = scanl (+) 0 (map length pr)
          -- pvals = intercalate "\n" $ zipWith (printVal) pr rowIdxs

  createPrList :: Integer -> [(Integer,Value)] -> [String]
  createPrList _ [] = []
  createPrList i [(j,v)] | i == j = [value v]
                         | otherwise = "0":createPrList (i+1) [(j,v)]
  createPrList i (x:xs) | i == (fst x) = (value (snd x)):createPrList (i+1) xs
                        | otherwise = "0":createPrList (i+1) (x:xs)

  createIndexMaps :: [(Integer,Value)] -> String
  createIndexMaps xs = intercalate "\n" (map createIndexMap groups)
    where groups = groupBy (\(_,v1) (_,v2) -> (value v1 == value v2)) xs

  createIndexMap :: [(Integer,Value)] -> String
  createIndexMap [] = ""
  createIndexMap [(i,v)] = "  Apr[" ++ show i ++ "] = p->" ++ value v ++ ";"
  createIndexMap ((i,v):xs) = intercalate "\n" $ [
        "  static const int " ++ s ++ "_ind_map[" ++ show n ++ "] = {" ++ inds ++ "};",
        assignment]
    where n = 1 + fromIntegral (length xs)
          m = (height v)*(width v)
          s = value v
          assignment 
            | width v == 1 && height v == 1 = intercalate "\n" $ [
              "  for (i = 0; i < " ++ show n ++ "; ++i) {",
              "    Apr[ " ++ s ++ "_ind_map[i] ] = p->" ++ s ++ ";",
              "  }"]
            | n == m = intercalate "\n" $ [
              "  ptr = (double *) p->" ++ s ++ ";",
              "  for (i = 0; i < " ++ show n ++ "; ++i) {",
              "    Apr[ " ++ s ++ "_ind_map[i] ] = *ptr++;",
              "  }"]
            | otherwise = intercalate "\n" $ map (\x -> intercalate "\n" $ [
              "  ptr = (double *) p->" ++ s ++ ";",
              "  for (i = "++ show x ++ "; i < " ++ show (m+x) ++ "; ++i) {",
              "    Apr[ " ++ s ++ "_ind_map[i] ] = *ptr++;",
              "  }"]) [0,m..m*(floor (fromIntegral n/(fromIntegral m) - 1))] -- n/(width*height) expected to be integer
                -- if a param shows up in multiple places, this will ensure that *ptr doesn't exceed its memory location
                -- it will reset the pointer, but start from a different map location
          inds = intercalate ", " ((show i):(map (show.fst) xs))


  -- specialized counters and getters
  countNNZ' :: Integer -> Integer -> Integer -> [(Integer,Integer)] -> [Integer]
  countNNZ' n i count []
    | i >= n = []
    | otherwise = count:(countNNZ' n (i+1) 0 [])
  countNNZ' n i count (x:xs)
    | snd x == i = countNNZ' n i (count + 1) xs
    | otherwise = count:(countNNZ' n (i+1) 0 (x:xs))   -- x should always be > i here since it's sorted

  countNNZ :: Integer -> Integer -> Integer -> [(Integer,Integer,Value)] -> [Integer]
  countNNZ n i count []
    | i >= n = []
    | otherwise = count:(countNNZ n (i+1) 0 [])
  countNNZ n i count ((r,c,v):xs)
    | c == i = countNNZ n i (count + 1) xs
    | otherwise = count:(countNNZ n (i+1) 0 ((r,c,v):xs))   -- x should always be > i here since it's sorted

  getCCSRow :: (Integer,Integer,Value) -> Integer
  getCCSRow (i,_,_) = i

  getCCSVal :: (Integer,Integer,Value) -> Value
  getCCSVal (_,_,p) = p

  valueOrder :: (Integer,Value) -> (Integer,Value) -> Ordering
  valueOrder (_,v1) (_,v2)  | (value v1) > (value v2) = GT
                            | (value v1) == (value v2) && (ri v1) > (ri v2) = GT
                            | (value v1) == (value v2) && (ri v1) == (ri v2) && (cj v1) > (cj v2) = GT
                            | otherwise = LT

  indexOrder :: (Integer,Value) -> (Integer,Value) -> Ordering
  indexOrder (i1,_) (i2,_) | i1 > i2 = GT
                            | otherwise = LT

  isConst :: (Integer,Value) -> Bool
  isConst (_,Const _ _ _ _ _) = True
  isConst _ = False

