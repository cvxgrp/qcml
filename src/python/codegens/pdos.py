from cvxopt import CVXOPTCodegen

class PDOSCodegen(CVXOPTCodegen):
    def function_preamble(self):
        yield "import cvxopt as o"
        yield "import cvxopt.solvers"
        yield "import pdos_direct"
        yield ""

    def function_solve(self):
        yield "dims['f'] = %s" % self.num_lineqs
        yield "b = o.matrix([b, h])"
        yield "A = o.sparse([A, G])"
        yield "opts = {'NORMALIZE': True, 'MAX_ITERS': 5000}"
        yield "sol = pdos_direct.solve(c, A, b, dims, opts)"

