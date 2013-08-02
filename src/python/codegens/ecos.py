from cvxopt import CVXOPTCodegen

class ECOSCodegen(CVXOPTCodegen):
    def function_preamble(self):
        yield "import cvxopt as o"
        yield "import ecos"
        yield ""

    def function_solve(self):
        yield "sol = ecos.ecos(c, G, h, dims, A, b)"


