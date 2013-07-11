from cvxopt import CVXOPTCodegen

class PDOSCodegen(CVXOPTCodegen):
    def function_preamble(self):
        return [
        "import cvxopt as _o",
        "import cvxopt.solvers",
        "import pdos_direct"
        ""]

    def function_solve(self):
        return ["_dims['f'] = %s" % self.num_lineqs,
            "_b = _o.matrix([_b, _h])",
            "_A = _o.sparse([_A, _G])",
            "_opts = {'NORMALIZE': True, 'MAX_ITERS': 5000}",
            "_sol = pdos_direct.solve(_c, _A, _b, _dims, _opts)"]

