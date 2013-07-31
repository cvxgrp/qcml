#from scoop.qc_ast import NodeVisitor, isscalar, RelOp, SOC, SOCProd
from codegen import PythonCodegen, ConstantCoeff, OnesCoeff, EyeCoeff, TransposeCoeff, ParameterCoeff

def cvxopt_eye(self):
    return "o.spmatrix(%s,range(%s),range(%s), tc='d')" % (self.coeff, self.n, self.n)

def cvxopt_ones(self):
    if self.transpose:
        return "o.matrix(%s,(1,%s), tc='d')" % (self.coeff, self.n)
    else:
        return "o.matrix(%s,(%s,1), tc='d')" % (self.coeff, self.n)

def cvxopt_trans(self):
    return "(%s).trans()" % self.arg

def cvxopt_parameter(self):
    return "params['%s']" % self.value



class CVXOPTCodegen(PythonCodegen):
    def __init__(self, dims):
        OnesCoeff.__str__ = cvxopt_ones
        EyeCoeff.__str__ = cvxopt_eye
        TransposeCoeff.__str__ = cvxopt_trans
        ParameterCoeff.__str__ = cvxopt_parameter
        super(CVXOPTCodegen,self).__init__(dims)

    def function_prototype(self):
        yield "def solve(params):" # % ', '.join(dims + params)]

    def function_preamble(self):
        yield "import cvxopt as o"
        yield "import cvxopt.solvers"
        yield "import itertools"
        yield ""

    def function_datastructures(self):
        """
            varlength
                length of x variable (as int)
            num_lineqs
                number of linear equality constraints (as int)
            num_lps
                number of linear inequality constraints (as int)
            num_conic
                number of SOC constraints (as int)
            cone_list
                list of *tuple* of SOC dimensions; tuple is (num, sz), with num
                being the multiplicity of the cone size, e.g. (2, 3) means *two*
                3-dimensional cones. it is a tuple of strings
        """
        def cone_tuple_to_str(x):
            num, sz = x
            if str(num) == '1':
                return "[%s]" % sz
            else:
                return "%s*[%s]" % (num, sz)
        cone_list_str = '[]'
        if self.cone_list:
            cone_list_str = map(cone_tuple_to_str, self.cone_list)
            cone_list_str = '+'.join(cone_list_str)

        yield "p, m, n = %d, %d, %d" % (self.num_lineqs, self.num_conic + self.num_lps, self.num_vars)
        yield "c = o.matrix(0, (n,1), tc='d')"
        yield "h = o.matrix(0, (m,1), tc='d')"
        yield "b = o.matrix(0, (p,1), tc='d')"
        yield "Gi, Gj, Gv = [], [], []"
        yield "Ai, Aj, Av = [], [], []"
        #"G = o.spmatrix([], [], [], (m,n), tc='d')",
        #"A = o.spmatrix([], [], [], (p,n), tc='d')",
        yield "dims = {'l': %d, 'q': %s, 's': []}" % (self.num_lps, cone_list_str)

    def function_solve(self):
        yield "sol = o.solvers.conelp(c, G, h, dims, A, b)"

    def function_recover(self,keys):
        # recover the old variables
        recover = (
            "'%s' : sol['x'][%s:%s]" % (k, self.varstart[k], self.varstart[k]+self.varlength[k])
                for k in keys
        )

        # TODO: recovers the objective value by computing c'*x, although it
        # really shouldn't. i'm just too lazy to fix this
        yield "# do the reverse mapping"
        yield "return {'info': sol, 'objval': %f*(c.trans()*sol['x'])[0] + %f, %s }" % (self.objective_multiplier, self.objective_offset, ', '.join(recover))

    def function_stuff_c(self, start, end, expr):
        yield "c[%s:%s] = %s" % (start, end, expr)

    def function_stuff_b(self, start, end, expr):
        yield "b[%s:%s] = %s" % (start, end, expr)

    def function_stuff_h(self, start, end, expr, stride = None):
        if stride is not None:
            yield "h[%s:%s:%s] = %s" % (start, end, stride, expr)
        else:
            yield "h[%s:%s] = %s" % (start, end, expr)

    def function_stuff_G(self, row_start, row_end, col_start, col_end, expr, row_stride = 1):
        n = (row_end - row_start)/row_stride
        if n > 1 and expr.isscalar:
            expr = OnesCoeff(n,ConstantCoeff(1))*expr
        to_sparse = expr.to_sparse()
        if to_sparse: yield to_sparse
        yield "Gi.append(%s)" % expr.I(row_start, row_stride)
        yield "Gj.append(%s)" % expr.J(col_start)
        yield "Gv.append(%s)" % expr.V()

    def function_stuff_A(self, row_start, row_end, col_start, col_end, expr, row_stride = 1):
        n = (row_end - row_start)/row_stride
        if n > 1 and expr.isscalar:
            expr = OnesCoeff(n,ConstantCoeff(1))*expr
        to_sparse = expr.to_sparse()
        if to_sparse: yield to_sparse
        yield "Ai.append(%s)" % expr.I(row_start, row_stride)
        yield "Aj.append(%s)" % expr.J(col_start)
        yield "Av.append(%s)" % expr.V()

    def function_stuff_spmat(self):
        """ Uses I, J, V to create G and A
        """
        yield "G = o.spmatrix(o.matrix(Gv), o.matrix(Gi), o.matrix(Gj), (m,n), tc='d')"
        yield "A = o.spmatrix(o.matrix(Av), o.matrix(Ai), o.matrix(Aj), (p,n), tc='d')"