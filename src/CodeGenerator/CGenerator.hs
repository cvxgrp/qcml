module CodeGenerator.CGenerator where
	import Expression.SOCP

	-- built on top of ECOS
	c_header :: String
	c_header = unlines $
		["/* stuff about open source license      */",
		 "/* ....                                 */",
		 "/* what does this solver solve?         */", -- missing ability to "show" problem
		 "/* Version 0.0.1                        */", -- version number goes here or something
		 "/* Eric Chu (c) 2012 or something...    */",
		 "",
		 "#ifndef __SOLVER_H__ // solver.h",
         "#define __SOLVER_H__",
         "#include <string.h> // for memcpy",
		 "#include \"ecos.h\"",
         "",
         "pwork* solver_init(list of params);",
         "",
         "#endif    // solver.h"]

	c_codegen :: SOCP -> String
	c_codegen x = unlines $
		["#include \"solver.h\""]

    c_data :: SOCP -> String
    c_data x = unlines $
        ["some fake parameter data for testing"]
