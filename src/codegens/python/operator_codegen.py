from .. base_operator_codegen import OperatorCodegen
from ... codes.function import PythonFunction
from ... codes.encoders import toPython
from ... properties.abstract_dim import AbstractDim

def wrap_self(f):
    def wrapped_code(self, *args, **kwargs):
        return f(*args, **kwargs)
    return wrapped_code

class PythonOperatorCodegen(OperatorCodegen):
    def __init__(self):
        super(PythonOperatorCodegen, self).__init__()
        self._code = {
            'fA': PythonFunction('fA', ['x']),
            'fG': PythonFunction('fG', ['x']),
            'fAT': PythonFunction('fAT', ['y']),
            'fGT': PythonFunction('fGT', ['y']),
            'prob2socp': PythonFunction('prob_to_socp', ['params', 'dims={}']),
            'socp2prob': PythonFunction('socp_to_prob', ['x', 'y', 'z', 'dims={}']),
        }
        self._codekeyorder = ['fA', 'fG', 'fAT', 'fGT', 'prob2socp', 'socp2prob']

    @property
    def prob2socp(self):
        return self.code['prob2socp']

    @property
    def socp2prob(self):
        return self.code['socp2prob']

    @property
    def fA(self):
        return self.code['fA']

    @property
    def fAT(self):
        return self.code['fAT']

    @property
    def fG(self):
        return self.code['fG']

    @property
    def fGT(self):
        return self.code['fGT']

    @property
    def extension(self):
        return ".py"

    # function to get problem dimensions
    def python_dimensions(self):
        yield "p, m, n = %s, %s, %s" % (self.num_lineqs, self.num_conic + self.num_lps, self.num_vars)

    # function to get cone dimensions
    def python_cone_sizes(self):
        def cone_tuple_to_str(x):
            num, sz = x
            if num == 1: return "[%s]" % sz
            else: return "%s*[%s]" % (num, sz)
        cone_list_str = '[]'
        if self.cone_list:
            cone_list_str = map(cone_tuple_to_str, self.cone_list)
            cone_list_str = '+'.join(cone_list_str)

        yield "cones = {'l': %s, 'q': %s, 's': []}" % (self.num_lps, cone_list_str)

    def python_recover(self):
        for k in self.program.variables.keys():
            start, length = self.primal_vars[k]
            yield "'%s' : x[%s:%s]" % (k, start, start+length)
        for k in self.dual_equality_vars.keys():
            start, length = self.dual_equality_vars[k]
            yield "'%s' : y[%s:%s]" % (k, start, start+length)
        for k in self.dual_conic_vars.keys():
            start, length = self.dual_conic_vars[k]
            yield "'%s' : z[%s:%s]" % (k, start, start+length)

    def functions_setup(self):
        # add some documentation
        self.prob2socp.document("maps 'params' into a dictionary of SOCP data")
        self.prob2socp.document("'params' ought to contain:")
        self.prob2socp.document(self.printshapes(self.program))

        # now import cvxopt and itertools
        self.prob2socp.add_lines("import numpy as np")
        self.prob2socp.add_lines("import scipy.sparse as sp")
        self.prob2socp.add_lines("import itertools")
        self.prob2socp.newline()
        # self.prob2socp.add_comment("convert possible numpy parameters to cvxopt matrices")
        # self.prob2socp.add_lines("from qcml.helpers import convert_to_cvxopt")
        # self.prob2socp.add_lines("params = convert_to_cvxopt(params)")
        # self.prob2socp.newline()

        # set up the data structures
        self.prob2socp.add_lines(self.python_dimensions())
        self.prob2socp.add_lines("c = np.zeros((n,))")
        self.prob2socp.add_lines("h = np.zeros((m,))")
        self.prob2socp.add_lines("b = np.zeros((p,))")
        # self.prob2socp.add_lines("Gi, Gj, Gv = [], [], []")
        # self.prob2socp.add_lines("Ai, Aj, Av = [], [], []")
        self.prob2socp.add_lines(self.python_cone_sizes())

        self.fA.add_lines("y = np.zeros((p,))")
        self.fAT.add_lines("x = np.zeros((n,))")
        self.fG.add_lines("y = np.zeros((m,))")
        self.fGT.add_lines("x = np.zeros((n,))")

    def functions_return(self):
        # TODO: what to do when m, n, or p is 0?
        # it "just worked" with CVXOPT, but not with scipy/numpy anymore...
        # self.prob2socp.add_comment("construct index and value lists for G and A")
        # self.prob2socp.add_lines("Gi = np.fromiter(itertools.chain.from_iterable(Gi), dtype=np.int)")
        # self.prob2socp.add_lines("Gj = np.fromiter(itertools.chain.from_iterable(Gj), dtype=np.int)")
        # self.prob2socp.add_lines("Gv = np.fromiter(itertools.chain.from_iterable(Gv), dtype=np.double)")
        # self.prob2socp.add_lines("Ai = np.fromiter(itertools.chain.from_iterable(Ai), dtype=np.int)")
        # self.prob2socp.add_lines("Aj = np.fromiter(itertools.chain.from_iterable(Aj), dtype=np.int)")
        # self.prob2socp.add_lines("Av = np.fromiter(itertools.chain.from_iterable(Av), dtype=np.double)")
        # self.prob2socp.add_lines("if m > 0: G = sp.csc_matrix((Gv, np.vstack((Gi, Gj))), (m,n))")
        # self.prob2socp.add_lines("else: G, h = None, None")
        # self.prob2socp.add_lines("if p > 0: A = sp.csc_matrix((Av, np.vstack((Ai, Aj))), (p,n))")
        # self.prob2socp.add_lines("else: A, b = None, None")

        self.fA.add_lines("return y")
        self.fAT.add_lines("return x")
        self.fG.add_lines("return y")
        self.fGT.add_lines("return x")

        self.prob2socp.add_lines(self.fA.code)
        self.prob2socp.newline()
        self.prob2socp.add_lines(self.fAT.code)
        self.prob2socp.newline()
        self.prob2socp.add_lines(self.fG.code)
        self.prob2socp.newline()
        self.prob2socp.add_lines(self.fGT.code)
        self.prob2socp.newline()

        self.prob2socp.add_lines("return {'c': c, 'G': fG, 'GT': fGT, 'h': h, 'A': fA, 'AT': fAT, 'b': b, 'dims': cones}")

        self.socp2prob.document("recovers the problem variables from the solver variable 'x' and dual variables 'y' (equality constraints) and 'z' (conic constraints)")
        # recover the old variables
        self.socp2prob.add_lines("return {%s}" % ', '.join(self.python_recover()))

    def stuff_c(self, start, end, expr):
        yield "c[%s:%s] = %s" % (start, end, toPython(expr))

    def stuff_b(self, start, end, expr):
        yield "b[%s:%s] = %s" % (start, end, toPython(expr))

    def stuff_h(self, start, end, expr, stride = None):
        if stride is not None:
            yield "h[%s:%s:%s] = %s" % (start, end, stride, toPython(expr))
        else:
            yield "h[%s:%s] = %s" % (start, end, toPython(expr))

    def stuff_function(self, func, rstart, rend, cstart, cend, expr, rstride):
        self.code[func].add_lines("y[%s:%s:%s] += %s * x[%s:%s]" % (rstart, rend, rstride, toPython(expr), cstart, cend))
        n = (rend - rstart) / rstride
        if (isinstance(n, AbstractDim) or n > 1) and expr.isscalar:
            self.code[func + 'T'].add_lines("x[%s:%s] += %s * np.sum(y[%s:%s:%s])" % (cstart, cend, toPython(expr.trans()), rstart, rend, rstride))
        else:
            self.code[func + 'T'].add_lines("x[%s:%s] += %s * y[%s:%s:%s]" % (cstart, cend, toPython(expr.trans()), rstart, rend, rstride))
        #yield expr

        # to_sparse = expr.to_sparse()
        # if to_sparse: yield toPython(to_sparse)
        # yield "%si.append(%s)" % (mat, toPython(expr.I(rstart, rstride)))
        # yield "%sj.append(%s)" % (mat, toPython(expr.J(cstart)))
        # yield "%sv.append(%s)" % (mat, toPython(expr.V()))

    def stuff_G(self, rstart, rend, cstart, cend, expr, rstride = 1):
        self.stuff_function("fG", rstart, rend, cstart, cend, expr, rstride)

    def stuff_A(self, rstart, rend, cstart, cend, expr, rstride = 1):
        self.stuff_function("fA", rstart, rend, cstart, cend, expr, rstride)

    def abstractdim_rewriter(self, ad):
        return "dims['%s']" % ad
