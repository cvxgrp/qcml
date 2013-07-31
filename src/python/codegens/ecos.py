from cvxopt import CVXOPTCodegen

class ECOSCodegen(CVXOPTCodegen):
    def function_preamble(self):
        return [
        "import cvxopt as o",
        "import ecos"
        ""]

    def function_solve(self):
        return ["sol = ecos.ecos(c, G, h, dims, A, b)"]


