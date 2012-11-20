module CodeGenerator.CGenerator(cHeader, cCodegen, cTestSolver, makefile, paramlist, varlist, paramsigns) where
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
  cHeader :: String -> String -> Codegen -> String
  cHeader ver desc x = unlines $
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
     "#include \"ecos.h\"",
     ""]
     ++ paramCode
     ++ varCode ++ [
     "pwork *setup(params *p);        /* setting up workspace (assumes params already declared) */",
     "int solve(pwork *w, vars *sol); /* solve the problem (assumes vars already declared) */",
     "void cleanup(pwork *w);         /* clean up workspace */",
     "",
     "#endif    /* solver.h */"]
    where 
      params = paramlist x
      paramCode = [paramStruct (paramlist x)]
      varCode = [variableStruct (varlist x)]
      --arglist = filter (/="") (map toArgs params)

  cCodegen :: Codegen -> String
  cCodegen x = unlines
    ["#include \"solver.h\"", "",
     setupFunc x,
     solverFunc x,
     cleanupFunc]  -- cleanup function is inlined

  -- although we'd like to generate "branch-free" code, for large-ish problems, this is not going to work
  cTestSolver :: Codegen -> String
  cTestSolver x = unlines $
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
     "  idxint i = 0;",
     "  idxint j = 0;",
     "  params p;",
     paramStrings,
     "  pwork *w = setup(&p);",
     "  int flag = 0;",
     "  if(w!=NULL) {",
     "    vars v;",
     "    flag = solve(w, &v);",
     "    cleanup(w);",
     "  }",
     "  return flag;",
     "}"]
     where params = paramlist x
           paramSizes = map ((\(x,y) -> x*y).dimensions) params
           n = cumsum paramSizes
           paramStrings = intercalate "\n" $ map expandParam params
           -- randVals = take n (randoms (mkStdGen seed) :: [Double])  -- TODO/XXX: doesn't take param signs in to account yet
           -- paraminits = intercalate "\n" [ s ++ " = " ++ (show v) ++ ";"  | (s,v) <- zip paramStrings randVals]

  makefile :: String -> Codegen -> String
  makefile ecos_path _ = unlines $
    [ "ECOS_PATH = " ++ ecos_path,
      "INCLUDES = -I$(ECOS_PATH)/code/include -I$(ECOS_PATH)/code/external/SuiteSparse_config",
      "LIBS = -lm $(ECOS_PATH)/code/libecos.a $(ECOS_PATH)/code/external/amd/libamd.a $(ECOS_PATH)/code/external/ldl/libldl.a",
      "",
      "all: solver.o",
      "\tgcc -Wall -O3 -o testsolver testsolver.c solver.o $(LIBS) $(INCLUDES)",
      "",
      "solver.o: solver.c",
      "\tgcc -ansi -Wall -O3 -c solver.c $(INCLUDES)",
      "",
      "clean:",
      "\trm testsolver *.o"]

  -- helper functions

  probDesc :: String -> String
  probDesc desc = intercalate ("\n") (map (" *     "++) (lines desc))

  -- XXX/TODO: varlist and paramlist may be special.... (may need to export them)
  varlist :: Codegen -> [Var]
  varlist c = catMaybes maybe_vars
    where maybe_vars = map (extractVar) (M.elems $ symbolTable c)

  paramlist :: Codegen -> [Param]
  paramlist c = catMaybes maybe_params
    where maybe_params = map (extractParam) (M.elems $ symbolTable c)

  paramsigns :: Codegen -> [Sign]
  paramsigns c = catMaybes maybe_signs
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
     ["int solve(pwork *w, vars *sol)",
     "{",
     "  idxint i = 0;",
     "  int exitflag = ECOS_solve(w);"]
     ++ zipWith expandVarIndices vars varInfo
     ++["  return exitflag;", "}"]
    where vars = varlist x -- variables in the problems
          varNames = map name vars
          varTable = buildVarTable x -- all variables introduced in problem rewriting
          varInfo = catMaybes $ map (flip lookup varTable) varNames

  expandVarIndices :: Var -> (Int, Int) -> String
  expandVarIndices v (ind, 1) = "  sol->" ++ (name v) ++ " = w->x["++ show ind ++"];"
  expandVarIndices v (ind, _) = intercalate "\n" [
    "  for(i = 0; i < " ++ show (rows v) ++ "; ++i) {",
    "    sol->"++ (name v) ++ "[i] = w->x[i + " ++ show ind ++ "];",
    "  }"] 
    --"  sol->" ++ (name v) ++ "["++ show i ++"] = w->x["++ show (ind + i) ++"];" | i <- [0..(rows v - 1)]]

    --"  memcpy(sol->" ++ (name v) ++ ", w->x + " ++ show ind ++ ", sizeof(double)*" ++ (show $ rows v) ++ ");"


  setupFunc :: Codegen -> String
  setupFunc x = unlines $ 
    ["pwork *setup(params *p)",
     "{",
     "  idxint i = 0;",
     "  static double b[" ++ show m ++ "]; /* = {0.0}; */",
     setBval p,
     "  static double c[" ++ show n ++ "]; /* = {0.0}; */",
     setCval p,
     "  static double h[" ++ show k ++ "]; /* = {0.0}; */",
     setQ higherDimCones,
     setG varTable p,
     setA varTable p,
     "  return ECOS_setup(" ++ arglist ++ ", q, Gpr, Gjc, Gir, Apr, Ajc, Air, c, h ,b);",
     "}"]
    where 
      p = problem x
      params = paramlist x
      varLens = getVariableRows p
      bLens = getBRows p
      coneLens = coneSizes p
      higherDimCones = sort $ filter (>1) coneLens
      n = cumsum varLens
      m = cumsum bLens
      k = cumsum coneLens
      l = k - (cumsum higherDimCones)
      arglist = intercalate ", " 
        [show n ++ " /* num vars */", 
         show k ++ " /* num cone constraints */", 
         show m ++ " /* num eq constraints */", 
         show l ++ " /* num linear cones */", 
         show (length higherDimCones) ++ " /* num second-order cones */"]
      varTable = buildVarTable x -- all variables introduced in problem rewriting


  setCval :: SOCP -> String
  setCval p = case (sense p) of
      Minimize -> "  c["++ show (n-1) ++ "] = 1;"
      Maximize -> "  c[" ++ show (n-1) ++ "] = -1;"
      Find -> ""
    where varLens = getVariableRows p
          n = cumsum varLens

  setBval :: SOCP -> String
  setBval p = intercalate "\n" $ catMaybes (zipWith expandBCoeff bCoeffs startIdxs)
    where bCoeffs = affine_b p
          bLens = getBRows p
          startIdxs = scanl (+) 0 bLens

  expandBCoeff :: Coeff -> Int -> Maybe String
  expandBCoeff (Ones n 0) idx = Nothing 
  expandBCoeff (Ones 1 x) idx = Just $ "  b[" ++ show idx ++ "] = " ++ show x ++ ";"
  expandBCoeff (Ones n x) idx = Just $ intercalate "\n" [
    "  for(i = " ++ show idx ++ "; i < " ++ show (idx+n) ++ "; ++i) {",
    "    b[i] = " ++ show x ++ ";",
    "  }"]
  expandBCoeff (Vector 1 p) idx = Just $ "  b[" ++ show idx ++ "] = p->" ++ (name p) ++ ";"
  expandBCoeff (Vector n p) idx = Just $ intercalate "\n" [
    "  for(i = " ++ show idx ++ "; i < " ++ show (idx+n) ++ "; ++i) {",
    "    b[i] = p->" ++ (name p) ++ "[i - " ++ show idx ++ "];",
    "  }"]
  expandBCoeff (VectorT 1 p) idx = Just $ "  b[" ++ show idx ++ "] = p->" ++ (name p) ++ ";"
  expandBCoeff (VectorT n p) idx = Just $ intercalate "\n" [
    "  for(i = " ++ show idx ++ "; i < " ++ show (idx+n) ++ "; ++i) {",
    "    b[i] = p->" ++ (name p) ++ "[0][i - " ++ show idx ++ "];",
    "  }"]
  --"  b[" ++ show (idx + i) ++ "] = p->" ++ (name p) ++ "[" ++ show i ++ "];" | i <- [0 .. (n - 1)]]
  expandBCoeff _ _ = Nothing

  cleanupFunc :: String
  cleanupFunc = unlines $ [
    "void cleanup(pwork *w)",
    "{",
    "  ECOS_cleanup(w,0);",
    "}"]

  setQ :: [Int] -> String
  setQ xs = "  static idxint q[" ++ show q ++ "] = {" ++ vals ++ "};"
    where vals = intercalate ", " (map show xs)
          q = length xs

  setG :: VarTable -> SOCP -> String
  setG table p = intercalate "\n" $
    ["  static idxint Gjc[" ++ show (n+1) ++ "] = {" ++ jc ++ "};",
     "  static idxint Gir[" ++ show nnz ++ "] = {" ++ ir ++ "};",
     "  static double Gpr[" ++ show nnz ++ "];", --" /* = {" ++ pr ++ "}; */",
     "  for(i = 0; i < " ++ show nnz ++ "; ++i) {",
     "    Gpr[i] = -1;",
     "  }"]
    where varLens = getVariableRows p
          n = cumsum varLens
          cones = cones_K p
          coneGroupSizes = map coneGroups cones
          -- generate permutation vector for cone groups (put smallest cones up front)
          forSortingCones = zip (map head coneGroupSizes) [1..(length coneGroupSizes)]
          pvec = map snd (sort forSortingCones)  -- permutation vector
          -- sort the cones and get the new sizes
          sortedCones = reorderCones pvec cones
          sortedSizes = map coneGroups sortedCones
          startIdxs = scanl (+) 0 (map cumsum sortedSizes)
          -- matrix G
          gmat = concat (zipWith (createCone table) sortedCones startIdxs) -- G in (i,j,-1) form
          matrixG = sortBy columnsOrder' gmat -- G in (i,j,-1) form, but sorted according to columns
          -- for printing
          nnz = length matrixG
          nnzPerRow = countNNZ' n 0 0 matrixG
          jc = intercalate ", " (map show (scanl (+) 0 nnzPerRow))
          ir = intercalate ", " (map (show.fst) matrixG)

  -- gets the list of cone sizes
  coneSizes :: SOCP -> [Int]
  coneSizes p = concat (map coneGroups (cones_K p))

  -- gets the cones associated with each variable, coneGroups (SOCelem [x,y,z]) = take (rows x) [3,...] 
  coneGroups :: SOC -> [Int]
  coneGroups (SOC vars) = [cumsum (map (\x -> (rows x)*(cols x)) vars)]
  coneGroups (SOCelem vars) = (take (rows (vars!!0)) (repeat $ length vars))

  --coneVars :: SOCP -> [Var]
  --coneVars p = concat (map vars (cones_K p))

  -- reorder the list of cones so that smallest ones are up front
  reorderCones :: [Int]->[SOC]->[SOC]
  reorderCones [] _ = []
  reorderCones _ [] = []
  reorderCones (p:ps) cones = val : reorderCones ps cones
    where val = cones!!(p - 1)

  -- produces (i,j,val) of nonzero locations in matrix G
  -- third argument "idx" is *row* start index
  createCone :: VarTable -> SOC -> Int -> [(Int,Int)]
  createCone table (SOC vars) idx = concat [expandCone i k | (i,k) <- zip idxs sizes ]
    where idxs = scanl (+) idx (map rows vars)
          sizes = catMaybes $ map (flip lookup table) (map name vars)
  createCone table (SOCelem vars) idx = concat [expandConeElem idx n i j k | (i,k) <- zip [0,1 ..] sizes, j <- [0 .. (m-1)]]
    where n = length vars
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
  expandConeElem :: Int -> Int -> Int -> Int -> (Int, Int) -> [(Int,Int)]
  expandConeElem idx n i j (k,l) = [(idx + i + j*n, k + j)] -- "G(" ++ show (idx + i + j*n) ++ ", " ++ show (k + j) ++ ") = -1;"

  expandCone :: Int -> (Int, Int) -> [(Int,Int)]
  expandCone idx (m,n) = [(idx +i, m+i) | i <- [0 .. (n-1)]] --intercalate "\n" ["G(" ++ show (idx + i) ++ ", " ++ show (m + i) ++ ") = -1" | i <- [0.. (n-1)]]


  setA :: VarTable -> SOCP -> String
  setA table p = compress n matrixA
    where varLens = getVariableRows p
          n = cumsum varLens
          a = affine_A p
          startIdxs = scanl (+) 0 (map rowHeight a)
          amat = concat (zipWith (createA table) a startIdxs)
          matrixA = sortBy columnsOrder amat

  rowHeight :: Row -> Int
  rowHeight r = maximum (map coeffRows (coeffs r))

  createA :: VarTable -> Row -> Int -> [(Int,Int,Value)]
  createA table row idx = concat [ expandARow i c s | (i,c,s) <- zip3 idxs coefficients sizes ]
    where vars = variables row
          coefficients = coeffs row
          sizes = catMaybes $ map (flip lookup table) (map name vars)
          idxs = getRowStartIdxs idx coefficients   -- only for concatenation

  getRowStartIdxs :: Int -> [Coeff] -> [Int]
  getRowStartIdxs idx c
    | all (==maxHeight) rowHeights = repeat idx
    | otherwise = scanl (+) 0 rowHeights   -- this currently works only because the maximum is guaranteed to be the *first* element
    where rowHeights = map coeffRows (tail c)
          maxHeight = coeffRows (head c)


  -- helper functions for generating CCS
  data Value = Value {
    value :: String,
    height :: Int,
    width :: Int,
    column :: Int,
    isTransposed :: Bool -- to help figure out how to emit the code
  } deriving (Eq)

  instance Show Value where
    show x = (value x) ++ "[" ++ show (column x) ++ "](" ++ show (height x) ++ ", " ++ show (width x) ++ ")" 

  expandARow :: Int -> Coeff -> (Int, Int) -> [(Int,Int,Value)]
  -- size of coeff should match
  expandARow idx (Eye _ x) (m,n) = [(idx + i, m + i, Value (show x) 1 1 1 False) | i <- [0 .. (n-1)]] -- eye length should equal m
  expandARow idx (Ones n x) (m,1) = [(idx + i, m, Value (show x) 1 1 1 False) | i <- [0 .. (n-1)]]  -- different pattern based on different coeff...
  expandARow idx (OnesT _ x) (m,n) = [(idx, m + i, Value (show x) 1 1 i False) | i <- [0 .. (n-1)]] -- onesT length should equal m
  expandARow idx (Diag n p) (m,_) = [(idx + i, m + i, toParamVal False i 0 p) | i <- [0 .. (n-1)]] -- onesT length should equal m
  expandARow idx (Matrix p) (m,n) = [(idx + i, m + j, toParamVal False i j p) | i <- [0 .. (rows p-1)], j <- [0 .. (cols p-1)]]
  expandARow idx (MatrixT p) (m,n) = [(idx + j, m + i, toParamVal True i j p) | i <- [0 .. (rows p-1)], j <- [0 .. (cols p-1)]]
  expandARow idx (Vector n p) (m,1) = [(idx + i, m, toParamVal False i 0 p) | i <- [0 .. (n-1)]]
  expandARow idx (VectorT _ p) (m,n) = [(idx, m + i, toParamVal True i 0 p) | i <- [0 .. (n-1)]]

  toParamVal :: Bool -> Int -> Int ->  Param -> Value
  toParamVal t _ _ (Param s (1,1)) = Value ("p->" ++ s) 1 1 1 t -- "p->" ++ s
  toParamVal t i _ (Param s (m,1)) = Value ("p->" ++ s) m 1 1 t -- "  p->" ++ s ++ "[" ++ show i ++ "]"
  toParamVal False i j (Param s (m,n)) = Value ("p->" ++ s) m n j False -- "p->" ++ s ++ "[" ++ show i ++ "]["++ show j ++ "]"
  toParamVal True i j (Param s (m,n)) = Value ("p->" ++ s) m n i True -- "p->" ++ s ++ "[" ++ show i ++ "]["++ show j ++ "]"


  -- sorting function for CCS
  columnsOrder' :: (Int,Int) -> (Int,Int) -> Ordering
  columnsOrder' (m1,n1) (m2,n2)  | n1 > n2 = GT
                                 | n1 == n2 && (m1 > m2) = GT
                                 | otherwise = LT

  columnsOrder :: (Int,Int,Value) -> (Int,Int,Value) -> Ordering
  columnsOrder (m1,n1,_) (m2,n2,_)  | n1 > n2 = GT
                                    | n1 == n2 && (m1 > m2) = GT
                                    | otherwise = LT

  -- compress (i,j,val) form in to column compressed form and outputs the string
  -- assumes (i,j,val) are sorted by columns
  compress :: Int -> [(Int,Int,Value)] -> String
  compress cols xs = intercalate "\n" $
    ["  static idxint Ajc[" ++ show (cols+1) ++ "] = {" ++ jc ++ "};",
     "  static idxint Air[" ++ show nnz ++ "] = {" ++ ir ++ "};",
     "  static double Apr[" ++ show nnz ++ "];", --" /* = {" ++ pr ++ "}; */",
     pvals] --pvals]
    where nnz = length xs
          nnzPerRow = countNNZ cols 0 0 xs
          jc = intercalate ", " (map show (scanl (+) 0 nnzPerRow))
          ir = intercalate ", " (map (show.getCCSRow) xs)
          pr = group (map getCCSVal xs)
          rowIdxs = scanl (+) 0 (map length pr)
          pvals = intercalate "\n" $ zipWith (printVal) pr rowIdxs


  -- massive HAX
  printVal vals idx
    | m == 1 && (height v == 1) && (width v == 1) = "  Apr[" ++ show idx ++ "] = " ++ (value v) ++ ";"
    | m == 1 && (height v == 1) = "  Apr[" ++ show idx ++ "] = " ++ expandValue idx v ++ ";"
    | otherwise = intercalate "\n" [
        "  for(i = " ++ show idx ++ "; i < " ++ show (idx + m) ++ "; ++i) {",
        "    Apr[i] = " ++ expandValue idx v ++ ";",
        "  }"]
        where m = length vals
              v = head vals

  expandValue :: Int -> Value -> String
  expandValue _ (Value s 1 1 1 _) = s
  expandValue idx (Value s m 1 1 _) = s ++ "[i - " ++ show idx ++ "]"
  expandValue _ (Value s 1 m i _) = s ++ "[0]["++ show i ++"]"
  expandValue idx (Value s m n j False) = s ++ "[i - " ++ show idx ++ "][" ++ show j ++ "]"
  expandValue idx (Value s m n j True) = s ++ "[" ++ show j ++ "][i - " ++ show idx ++ "]"


  -- specialized counters and getters
  countNNZ' :: Int -> Int -> Int -> [(Int,Int)] -> [Int]
  countNNZ' n i count []
    | i >= n = []
    | otherwise = count:(countNNZ' n (i+1) 0 [])
  countNNZ' n i count (x:xs)
    | snd x == i = countNNZ' n i (count + 1) xs
    | otherwise = count:(countNNZ' n (i+1) 0 (x:xs))   -- x should always be > i here since it's sorted

  countNNZ :: Int -> Int -> Int -> [(Int,Int,Value)] -> [Int]
  countNNZ n i count []
    | i >= n = []
    | otherwise = count:(countNNZ n (i+1) 0 [])
  countNNZ n i count ((r,c,v):xs)
    | c == i = countNNZ n i (count + 1) xs
    | otherwise = count:(countNNZ n (i+1) 0 ((r,c,v):xs))   -- x should always be > i here since it's sorted

  getCCSRow :: (Int,Int,Value) -> Int
  getCCSRow (i,_,_) = i

  getCCSVal :: (Int,Int,Value) -> Value
  getCCSVal (_,_,p) = p

