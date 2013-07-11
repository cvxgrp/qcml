from cvxopt import CVXOPTCodegen
from qcml.qc_ast import RelOp, SOC, SOCProd

class ECOSCodegen(CVXOPTCodegen):
    def function_preamble(self):
        return [
        "import cvxopt as _o",
        "import ecos"
        ""]

    def function_solve(self):
        return ["_sol = ecos.ecos(_c, _G, _h, _dims, _A, _b)"]


