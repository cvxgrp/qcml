module CodeGenerator.CGenerator(cHeader, cCodegen, cData) where
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
     "",
     "// clean up workspace",
     "static inline void cleanup(pwork *w)",
     "{",
     "  ECOS_cleanup(w,0);",
     "}",
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
     solverFunc x]  -- cleanup function is inlined

  cData :: Codegen -> String
  cData x = unlines $
    ["some fake parameter data for testing"]

  probDesc :: String -> String
  probDesc desc = intercalate ("\n") (map (" *     "++) (lines desc))


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
          varLens = getVariableSizes p
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
  expandVarIndices v (Just (ind, _)) = "  memcpy(sol->" ++ (name v) ++ ", w->x + " ++ show ind ++ ", sizeof(double)*" ++ (show $ rows v) ++ ");"


  setupFunc :: Codegen -> String
  setupFunc x = unlines $ 
    ["pwork *setup(params *p)",
     "{",
     "  // stuff matrices and stuff, then...",
     "  static double c[" ++ show n ++ "];",
     setCval x,
     "  static double h[100];",
     "  return ECOS_setup(N,M,P,L,NCONES,Q, gpr, gjc, gir, apr, ajc, air, c, h ,b);",
     "  // uppercase are known during codegen time, lowercase are results of stuffing",
     "}"]
    where 
      params = paramlist x
      varLens = getVariableSizes (problem x)
      n = cumsum varLens

  setCval :: Codegen -> String
  setCval x = case (sense p) of
      Minimize -> "  c["++ show (n-1) ++ "] = 1;"
      Maximize -> "  c[" ++ show (n-1) ++ "] = -1;"
      Find -> ""
    where p = problem x
          varLens = getVariableSizes p
          n = cumsum varLens

  cumsum = foldl (+) 0
