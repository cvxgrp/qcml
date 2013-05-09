from codegen import Eye, Ones, Transpose, Codegen
from cvxopt import CVXOPTCodegen, cvxopt_eye, cvxopt_ones, cvxopt_trans
from dimensions import Dimension
from scoop.qc_ast import RelOp, SOC, SOCProd


Ones.__str__ = cvxopt_ones
Eye.__str__ = cvxopt_eye
Transpose.__str__ = cvxopt_trans

class ECOSCodegen(CVXOPTCodegen):    
    def function_preamble(self):
        return [
        "\"\"\"",
        "Default solves scalar problem",
        "     find x",
        "     s.t. x <= 0",
        "\"\"\"",
        "import cvxopt as _o",
        "import cvxopt.solvers",
        "import ecos"
        ""]
    
    def function_solve(self):
        return ["_sol = ecos.ecos(_c, _G, _h, _dims, _A, _b)"]
    

  