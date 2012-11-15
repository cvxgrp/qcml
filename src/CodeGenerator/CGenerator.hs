module CodeGenerator.CGenerator(cHeader, cCodegen, cData, paramlist, varlist) where
  import CodeGenerator.Common
  import Expression.Expression
  import qualified Data.Map as M

  import Data.Maybe
  -- TODO/XXX: if we know the target architecture, we can make further optimizations such
  -- as memory alignment. but as it stands, we're just generating flat C

  -- TODO/XXX: need to figure out what Alex's "minimum" package for ECOS is so it can be
  -- distributed alongside the generated code

  -- TODO/XXX: annotate sparsity structure (assume it's known at "compile" time)

  -- built on top of ECOS
  cHeader :: String -> Codegen -> String
  cHeader desc x = unlines $
    ["/* stuff about open source license",
     " * ....",
     " * The problem specification for this solver is: ",
     " *", 
     probDesc desc,
     " *",
     " * For now, parameters are *dense*, and we don't respect sparsity. The sparsity",
     " * structure has to be specified during code generation time. We could do the more",
     " * generic thing and allow sparse matrices, but as a first cut, we won't.",
     " * Version 0.0.1", -- version number goes here or something
     " * Eric Chu, Alex Domahidi, Neal Parikh, Stephen Boyd (c) 2012 or something...",
     " */",
     "",
     "#ifndef __SOLVER_H__ // solver.h",
     "#define __SOLVER_H__",
     "",
     "#include <string.h> // for memcpy",
     "#include \"ecos.h\"",
     ""]
     ++ paramCode
     ++ varCode ++ [
     "pwork *setup(params *p);        // setting up workspace (assumes params already declared)",
     "int solve(pwork *w, vars *sol); // solve the problem (assumes vars already declared)",
     "void cleanup(pwork *w);         // clean up workspace",
     "",
     "#endif    // solver.h"]
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

  cData :: Codegen -> String
  cData x = unlines $
    ["some fake parameter data for testing"]

  probDesc :: String -> String
  probDesc desc = intercalate ("\n") (map (" *     "++) (lines desc))

  -- XXX/TODO: varlist and paramlist may be special.... (may need to export them)
  varlist :: Codegen -> [Var]
  varlist c = catMaybes maybe_vars
    where maybe_vars = map (extractVar) (M.elems $ symbolTable c)

  paramlist :: Codegen -> [Param]
  paramlist c = catMaybes maybe_params
    where maybe_params = map (extractParam) (M.elems $ symbolTable c)

  extractVar :: Expr -> Maybe Var
  extractVar (Variable v) = Just v
  extractVar _ = Nothing

  extractParam :: Expr -> Maybe Param
  extractParam (Parameter v _) = Just v
  extractParam _ = Nothing

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
  toParamString (Param s (1,1) _) = "  double " ++ s ++ ";"
  toParamString (Param s (m,1) _) = "  double " ++ s ++ "[" ++ show m ++ "];"
  -- toParamString (Param s (1,m) _) = "  double " ++ s ++ "[" ++ show m ++ "];"
  toParamString (Param s (m,n) False) = "  double " ++ s ++ "[" ++ show m ++ "]["++ show n ++ "];"
  toParamString (Param s (m,n) True) = "  double " ++ s ++ "[" ++ show n ++ "]["++ show m ++ "];"


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
     "  int exitflag = ECOS_solve(w);"]
     ++ zipWith expandVarIndices vars varInfo
     ++["  return exitflag;", "}"]
    where vars = varlist x -- variables in the problems
          varNames = map name vars
          varTable = buildVarTable x -- all variables introduced in problem rewriting
          varInfo = map (flip lookup varTable) varNames

  expandVarIndices :: Var -> Maybe (Int, Int) -> String
  expandVarIndices _ Nothing = ""
  expandVarIndices v (Just (ind, _)) = intercalate "\n" ["  sol->" ++ (name v) ++ "["++ show i ++"] = w->x["++ show (ind + i) ++"];" | i <- [0..(rows v - 1)]]

    --"  memcpy(sol->" ++ (name v) ++ ", w->x + " ++ show ind ++ ", sizeof(double)*" ++ (show $ rows v) ++ ");"


  setupFunc :: Codegen -> String
  setupFunc x = unlines $ 
    ["pwork *setup(params *p)",
     "{",
     "  double b[" ++ show m ++ "] = {0.0};",
     setBval p,
     "  double c[" ++ show n ++ "] = {0.0};",
     setCval p,
     "  double h[" ++ show k ++ "] = {0.0};",
     setQ higherDimCones,
     setG varTable p,
     "  return ECOS_setup(" ++ arglist ++ ", q, Gpr, Gjc, Gir, Apr, Ajc, Air, c, h ,b);",
     "  // uppercase are known during codegen time, lowercase are results of stuffing",
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
  setBval p = intercalate "\n" $ catMaybes (zipWith expandCoeff bCoeffs startIdxs)
    where bCoeffs = affine_b p
          bLens = getBRows p
          startIdxs = scanl (+) 0 bLens

  expandCoeff :: Coeff -> Int -> Maybe String
  expandCoeff (Ones n 0) idx = Nothing 
  expandCoeff (Ones 1 x) idx = Just $ "  b[" ++ show idx ++ "] = " ++ show x ++ ";"
  expandCoeff (Ones n x) idx = Just $ intercalate "\n" ["  b[" ++ show (idx + i) ++ "] = " ++ show x ++ ";" | i <- [0 .. (n - 1)]]
  expandCoeff (Vector n p) idx = Just $ intercalate "\n" ["  b[" ++ show (idx + i) ++ "] = p->" ++ (name p) ++ "[" ++ show i ++ "];" | i <- [0 .. (n - 1)]]
  expandCoeff _ _ = Nothing

  cleanupFunc :: String
  cleanupFunc = unlines $ [
    "void cleanup(pwork *w)",
    "{",
    "  ECOS_cleanup(w,0);",
    "}"]

  setQ :: [Int] -> String
  setQ xs = "  int q[" ++ show q ++ "] = {" ++ vals ++ "};"
    where vals = intercalate ", " (map show xs)
          q = length xs

  setG :: VarTable -> SOCP -> String
  setG table p = compress n matrixG
    where varLens = getVariableRows p
          n = cumsum varLens
          cones = cones_K p
          coneGroupSizes = map coneGroups cones
          -- generate permutation vector for cone groups (put smallest cones up front)
          forSortingCones = zip (map head coneGroupSizes) [1..(length coneGroupSizes)]
          pvec = map snd (sort forSortingCones)  -- permutation vector
          sortedCones = reorderCones pvec cones
          sortedSizes = map coneGroups sortedCones
          startIdxs = scanl (+) 0 (map cumsum sortedSizes)
          matrixG = concat (zipWith (createCone table) sortedCones startIdxs) -- G in (i,j,val) form

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
  createCone :: VarTable -> SOC -> Int -> [(Int,Int,Int)]
  createCone table (SOC vars) idx = concat [expandCone i k | (i,k) <- zip idxs sizes ]
    where idxs = scanl (+) idx (map rows vars)
          sizes = map (flip lookup table) (map name vars)
  createCone table (SOCelem vars) idx = concat [expandConeElem idx n i j k | (i,k) <- zip [0,1 ..] sizes, j <- [0 .. (m-1)]]
    where n = length vars
          m = rows (vars!!0)
          sizes = map (flip lookup table) (map name vars)

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
  expandConeElem :: Int -> Int -> Int -> Int -> Maybe (Int,Int) -> [(Int,Int,Int)]
  expandConeElem _ _ _ _ Nothing = []
  expandConeElem idx n i j (Just (k,l)) = [(idx + i + j*n, k + j,-1)] -- "G(" ++ show (idx + i + j*n) ++ ", " ++ show (k + j) ++ ") = -1;"

  expandCone :: Int -> Maybe (Int, Int) -> [(Int,Int,Int)]
  expandCone _ Nothing = []
  expandCone idx (Just (m,n)) = [(idx +i, m+i,-1) | i <- [0 .. (n-1)]] --intercalate "\n" ["G(" ++ show (idx + i) ++ ", " ++ show (m + i) ++ ") = -1" | i <- [0.. (n-1)]]


  compress :: Int -> [(Int,Int,Int)] -> String
  compress cols xs = intercalate "\n" $
    ["  int Gjc[" ++ show cols ++ "] = {" ++ gjc ++ "};",
     "  int Gir[" ++ show nnz ++ "] = {" ++ gir ++ "};",
     "  double Gpr[" ++ show nnz ++ "] = {" ++ gpr ++ "};"]
    where counter = take cols (repeat 0)
          nnz = length xs
          nnzPerRow = foldl countNNZ counter xs
          gjc = intercalate ", " (map show (scanl (+) 0 nnzPerRow))
          gir = intercalate ", " (map show (map getCCSRow xs))
          gpr = intercalate ", " (map show (map getCCSVal xs))

  countNNZ :: [Int] -> (Int,Int,Int) -> [Int]
  countNNZ count (_,c,_) = front ++ [new] ++ back
    where (front, rest) = splitAt c count
          back = tail rest
          new = (head rest)+ 1

  getCCSRow :: (Int,Int,Int) -> Int
  getCCSRow (i,_,_) = i

  getCCSVal :: (Int,Int,Int) -> Int
  getCCSVal (_,_,p) = p

