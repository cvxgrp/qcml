module CodeGenerator.CGenerator(c_header, c_codegen, c_data) where
  import CodeGenerator.Common
  import Expression.Expression
  import qualified Data.Map as M

  import Data.Maybe
  -- TODO/XXX: if we know the target architecture, we can make further optimizations such
  -- as memory alignment. but as it stands, we're just generating flat C

  -- built on top of ECOS
  c_header :: String -> Codegen -> String
  c_header desc x = unlines $
    ["/* stuff about open source license",
     " * ....",
     " * The problem specification for this solver is: ",
     " *", 
     prob_desc desc,
     " *",
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
     ++ mat_code -- only if there are matrix params!
     ++ vec_code -- only if there are vector params!
     ++ var_code
     ++ ["pwork* setup(" ++ (intercalate ", " arglist) ++ "); // setting up workspace",
     "vars_t solve(pwork* w); // solve the problem",
     "void cleanup(pwork *w);",
     "",
     "#endif    // solver.h"]
    where 
      params = paramlist x
      (mat_code, vec_code)
        | null params = ([], [])
        | otherwise = ([matrix_param_struct], [vector_param_struct])
      var_code = [variable_struct (varlist x)]
      arglist = filter (/="") (map toArgs params)

  c_codegen :: Codegen -> String
  c_codegen x = unlines $
    ["#include \"solver.h\"",
     "",
     "pwork* setup("++ (intercalate ", " arglist) ++")",
     "{",
     "  // stuff matrices and stuff, then...",
     "  return ECOS_setup(N,M,P,L,NCONES,Q, gpr, gjc, gir, apr, ajc, air, c,h ,b);",
     "  // uppercase are known during codegen time, lowercase are results of stuffing",
     "}",
     "",
     "vars_t solve(pwork *w)",
     "{",
     "  int exitflag = ECOS_solve(w);",
     "  vars_t solution;",
     "  solution.x = w.x + sizeof(pfloat)*OFFSET;",
     "  return solution;",
     "}",
     "",
     "void cleanup(pwork *w)",
     "{",
     "  ECOS_cleanup(w,0);",  -- cleans up *all* memory, orphans pointers in vars_t
     "  // what about setting vars to null??",
     "}"]
    where 
      params = paramlist x
      arglist = filter (/="") (map toArgs params)

  c_data :: Codegen -> String
  c_data x = unlines $
    ["some fake parameter data for testing"]

  prob_desc :: String -> String
  prob_desc desc = intercalate ("\n") (map (" *     "++) (lines desc))

  -- these are pass by value (which may, or may not be a good idea)
  toArgs :: Param -> String
  toArgs p = case (shape p) of
    (1,1) -> "double " ++ (name p)
    (_,1) -> "vec_t " ++ (name p)
    otherwise -> "mat_t " ++ (name p)

  varlist :: Codegen -> [Var]
  varlist c = catMaybes maybe_vars
    where maybe_vars = map (extract_var) (M.elems $ symbolTable c)

  paramlist :: Codegen -> [Param]
  paramlist c = catMaybes maybe_params
    where maybe_params = map (extract_param) (M.elems $ symbolTable c)

  extract_var :: Expr -> Maybe Var
  extract_var (Variable v) = Just v
  extract_var _ = Nothing

  extract_param :: Expr -> Maybe Param
  extract_param (Parameter v _) = Just v
  extract_param _ = Nothing


  matrix_param_struct :: String
  matrix_param_struct = unlines
     ["/*",
      " * struct mat_t",
      " * ============",
      " * This is a sparse matrix in column compressed storage.",
      " *",
      " * As an example, consider the matrix:",
      " *     | 10   0  -2   0 |",
      " * A = |  3   0   0  -2 |",
      " *     |  0   0   5   0 |",
      " *",
      " * In this example,",
      " *     length(jc) = (# of columns) + 1 = 4 + 1, jc = {0,2,2,4,5},",
      " *     length(ir) = nnz(A) = 5, ir = {0,1,0,2,1},",
      " *     length(pr) = nnz(A) = 5, pr = {10,3,-2,5,-2}",
      " * By convention, jc[end] = nnz(A).",
      " * ",
      " * Other sparse matrices in column compressed storage can be copied",
      " * to this format by copying pointers. Note that the dimensions of the",
      " * matrices are not needed in this data structure.",
      " */",
      "typedef struct mat_t {",
      "  int* jc;     // column pointer",
      "  int* ir;     // row index",
      "  double* pr;  // values",
      "};"]

  vector_param_struct :: String
  vector_param_struct = unlines
    ["/*",
     " * struct vec_t",
     " * ============",
     " * This is a sparse vector in column compressed storage.",
     " *",
     " * Only the nonzero values are stored along with their row index.",
     " * This storage can be thought of as the pair (ind, val). Note that",
     " * the length of the vector is not needed in this data structure. ",
     " */",
     "typedef struct vec_t {",
     "  int nnz;     // nnz in vector",
     "  int *ir;     // row index",
     "  double *pr;  // values",
     "};"]

  -- XXX: vector sizes are missing... lol
  variable_struct :: [Var] -> String
  variable_struct variables = unlines $ 
    ["/*",
     " * struct vars_t",
     " * =============",
     " * This structure stores the solution variables for your problem.",
     " */",
     "typedef struct vars_t {"]
     ++ map (\x -> "  double *" ++ name x ++ ";") variables ++
     [
    -- "  double *dualvars;", -- TODO: dual vars
     "};"]

