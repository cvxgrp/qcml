module CodeGenerator.CGenerator(c_header) where
	import CodeGenerator.Common
	import Expression.SOCP

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
		 ++ ["pwork* solver_init(" ++ (intercalate ", " arglist) ++ ");",
		 "",
		 "#endif    // solver.h"]
		where 
			prob = problem x
			params = prob_params prob
			param_names = map (\(_,_,s)->s) params
			mat_code
				| any (\(_,n,_) -> (n > 1)) params = [matrix_param_struct]
				| otherwise = []
			vec_code
				| any (\(m,n,_) -> (m > 1) && (n==1)) params = [vector_param_struct]
				| otherwise = []
			arglist = filter (/="") (map toArgs params)

	c_codegen :: SOCP -> String
	c_codegen x = unlines $
		["#include \"solver.h\""]

	c_data :: SOCP -> String
	c_data x = unlines $
		["some fake parameter data for testing"]

	prob_desc :: String -> String
	prob_desc desc = intercalate ("\n") (map (" *     "++) (lines desc))

	-- by the time we're here, we've lost the sign information (is that OK?)
	-- also by the time we're here, transposed parameters are *named* with postfix " ' "
	-- it's not terrible, but there's a question of whether transposed params should have their own
	-- data type
	prob_params :: SOCP -> [(Int,Int,String)]
	prob_params prob = (filter (/=(0,0,"")) coefficients)
		where
			coefficients = map coeff_param aParams ++ map coeff_param bParams
			aParams = concat $ map coeffs (affine_A prob)
			bParams = affine_b prob
	
	coeff_param :: Coeff -> (Int, Int, String)
	coeff_param (Diag m s) = (m,m,s)
	coeff_param (Matrix (m,n) s) = (m,n,s)
	coeff_param (Vector m s) = (m,1,s)
	coeff_param _ = (0,0,"")

	param_name :: Coeff -> String
	param_name = (\(_,_,s) -> s).coeff_param

	-- these are pass by value (which may, or may not be a good idea)
	toArgs :: (Int, Int, String) -> String
	toArgs (_,_,"") = ""
	toArgs (1,1,s) = "double " ++ s
	toArgs (_,1,s) = "Vec_t " ++ s
	toArgs (_,_,s) = "Mat_t " ++ s

	matrix_param_struct :: String
	matrix_param_struct = unlines
		 ["/*",
		  " * struct Mat_t",
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
		  "typedef struct Mat_t {",
		 	"  idxint* jc;  // column pointer",
   	 	"  idxint* ir;  // row index",
   	 	"  pfloat* pr;  // values",
   	 	"};"]

	vector_param_struct :: String
	vector_param_struct = unlines
		["/*",
		 " * struct Vec_t",
		 " * ============",
		 " * This is a sparse vector in column compressed storage.",
		 " *",
		 " * Only the nonzero values are stored along with their row index.",
		 " * This storage can be thought of as the pair (ind, val). Note that",
		 " * the length of the vector is not needed in this data structure. ",
		 " */",
		 "typedef struct Vec_t {",
		 "  idxint *ir;  // row index",
		 "  pfloat *pr;  // values",
		 "};"]


