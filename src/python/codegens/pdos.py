from cvxopt import CVXOPTCodegen

class PDOSCodegen(CVXOPTCodegen):
    def function_preamble(self):
        return [
        "import cvxopt as o",
        "import cvxopt.solvers",
        "import pdos_direct"
        ""]

    def function_solve(self):
        return ["dims['f'] = %s" % self.num_lineqs,
            "b = o.matrix([b, h])",
            "A = o.sparse([A, G])",
            "opts = {'NORMALIZE': True, 'MAX_ITERS': 5000}",
            "sol = pdos_direct.solve(c, A, b, dims, opts)"]

