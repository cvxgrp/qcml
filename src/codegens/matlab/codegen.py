from .. base_codegen import Codegen
from ... codes import OnesCoeff
from ... codes.function import MatlabFunction
from ... codes.encoders import toMatlab
from ... properties.abstract_dim import AbstractDim

class MatlabCodegen(Codegen):
    def __init__(self):
        super(MatlabCodegen, self).__init__()
        self._code = {
            'wrap': MatlabFunction('qc_wrap', ['params', 'dims'], ['vars', 'optval']),
            'prob2socp': MatlabFunction('prob_to_socp', ['params', 'dims'], ['data']),
            'socp2prob': MatlabFunction('socp_to_prob', ['x', 'dims'], ['vars']),
        }
        self._codekeyorder = ['wrap', 'prob2socp', 'socp2prob']

    @property
    def prob2socp(self): return self.code['prob2socp']

    @property
    def socp2prob(self): return self.code['socp2prob']

    @property
    def extension(self):
        return ".m"

    @property
    def wrap(self): return self.code['wrap']

    def conesq(self):
        def cone_tuple_to_str(x):
            num, sz = x
            if num == 1: return str(sz)
            else:        return "%s*ones(%s,1)" % (sz, num)
        yield '; '.join(map(cone_tuple_to_str, self.cone_list))

    def functions_setup(self):
        self.prob2socp.document('PROB2SOCP: maps PARAMS into a struct of SOCP matrices')
        self.prob2socp.document('Where input struct PARAMS has the following fields:')
        self.prob2socp.document(self.printshapes(self.program))
        self.prob2socp.newline()

        self.prob2socp.add_lines("p = %s; m = %s; n = %s;" % v for v in self.pmn)
        self.prob2socp.add_lines("c = zeros(n,1);")
        self.prob2socp.add_lines("h = zeros(m,1);")
        self.prob2socp.add_lines("b = zeros(p,1);")
        self.prob2socp.add_lines("Gi = []; Gj = []; Gv = [];")
        self.prob2socp.add_lines("Ai = []; Aj = []; Av = [];")
        self.prob2socp.newline()

        self.prob2socp.add_lines("cones.l = %s;"   % l for l in self.conesl)
        self.prob2socp.add_lines("cones.q = [%s];" % q for q in self.conesq())
        self.prob2socp.add_lines("cones.s = [];")

    def functions_return(self):
        self.prob2socp.add_comment('Convert from sparse triplet to column compressed format.')
        self.prob2socp.add_comment('Also convert from 0 indexed to 1 indexed.')
        self.prob2socp.add_lines("A = sparse(Ai+1, Aj+1, Av, p, n);")
        self.prob2socp.add_lines("G = sparse(Gi+1, Gj+1, Gv, m, n);")
        self.prob2socp.newline()
        self.prob2socp.add_comment('Build output')
        self.prob2socp.add_lines("data = struct('c', c, 'b', b, 'h', h, 'G', G, 'A', A, 'dims', cones);")

        recover = (
            "'%s', x(%s:%s)" % (k, 1+self.varstart[k], self.varstart[k]+self.varlength[k])
            for k in self.program.variables.keys()
        )
        self.socp2prob.add_lines("vars = struct(%s);" % ', '.join(recover))

        self.wrap.add_lines("data = prob2socp(params, dims);")
        self.wrap.add_lines("[x,y,info,s,z] = ECOS(data.c, data.G, data.h, data.dims, data.A, data.b);")
        self.wrap.add_lines("vars = socp2prob(x, dims);")
        self.wrap.add_lines("optval = x' * b;")

    def stuff_vec(self, vec, start, end, expr, stride):
        """ Stuffing here is 1 indexed, even though in matlab_encoder we stay
            0 indexed.  Hopefully this can be cleaned up!
        """
        if stride == 1:
            yield "%s(%s:%s) = %s;" % (vec, start+1, end, toMatlab(expr))
        else:
            yield "%s(%s:%s:%s) = %s;" % (vec, start+1, stride, end, toMatlab(expr))

    def stuff_c(self, start, end, expr, stride = 1):
        return self.stuff_vec("c", start, end, expr, stride)

    def stuff_b(self, start, end, expr, stride = 1):
        return self.stuff_vec("b", start, end, expr, stride)

    def stuff_h(self, start, end, expr, stride = 1):
        return self.stuff_vec("h", start, end, expr, stride)

    def stuff_matrix(self, mat, r0, rend, c0, cend, expr, rstride):
        n = (rend - r0) / rstride

        if (isinstance(n, AbstractDim) or n > 1) and expr.isscalar:
            expr = OnesCoeff(n, expr)
        to_sparse = expr.to_sparse()
        if to_sparse: yield toMatlab(to_sparse)
        yield "%si = [%si; %s];" % (mat, mat, toMatlab(expr.I(r0, rstride)))
        yield "%sj = [%sj; %s];" % (mat, mat, toMatlab(expr.J(c0)))
        yield "%sv = [%sv; %s];" % (mat, mat, toMatlab(expr.V()))

    def stuff_A(self, r0, rend, c0, cend, expr, rstride = 1):
        return self.stuff_matrix("A", r0, rend, c0, cend, expr, rstride)

    def stuff_G(self, r0, rend, c0, cend, expr, rstride = 1):
        return self.stuff_matrix("G", r0, rend, c0, cend, expr, rstride)

    def abstractdim_rewriter(self, ad):
        return "dims.%s" % ad
