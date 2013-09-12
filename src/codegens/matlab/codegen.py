from .. base_codegen import Codegen
from qcml.codes.coefficients import coefficient
from qcml.codes.function import MatlabFunction
import qcml.codes.encoders as encoder

class MatlabCodegen(Codegen):
    def __init__(self, dims):
        super(MatlabCodegen, self).__init__(dims)
        self.__prob2socp = MatlabFunction("prob_to_socp", ["params"], ["data"])
        self.__socp2prob = MatlabFunction("socp_to_prob", ["x"],      ["vars"])

    @property
    def prob2socp(self): return self.__prob2socp

    @property
    def socp2prob(self): return self.__socp2prob

    def dimsq(self):
        def cone_tuple_to_str(x):
            num, sz = x
            if num == 1: return str(sz)
            else:        return "%s*ones(%s,1)" % (sz, num)
        yield '; '.join(map(cone_tuple_to_str, self.cone_list))

    def functions_setup(self, program_node):
        self.prob2socp.document(self.printshapes(program_node))

        # Add stuff to ensure that params are in columns not rows?
        self.prob2socp.add_lines("p = %d; m = %d; n = %d" % v for v in self.pmn)
        self.prob2socp.add_lines("c = zeros(n,1);")
        self.prob2socp.add_lines("h = zeros(m,1);")
        self.prob2socp.add_lines("b = zeros(p,1);")
        self.prob2socp.add_lines("Gi = []; Gj = []; Gv = [];")
        self.prob2socp.add_lines("Ai = []; Aj = []; Av = [];")
        self.prob2socp.add_lines("dims.l = %d;" % l for l in self.dimsl)
        self.prob2socp.add_lines("dims.q = [%s];" % q for q in self.dimsq())
        self.prob2socp.add_lines("dims.s = [];")

    def functions_return(self, program_node):
        self.prob2socp.add_lines("A = sparse(Ai+1, Aj+1, Av);")
        self.prob2socp.add_lines("G = sparse(Gi+1, Gj+1, Gv);")
        self.prob2socp.add_lines("data = struct('c', c, 'b', b, 'h', h, 'G', G, 'A', A, 'dims', dims);")

        recover = (
            "'%s', x(%s:%s)" % (k, self.varstart[k], self.varstart[k]+self.varlength[k])
            for k in program_node.variables.keys()
        )
        self.socp2prob.add_lines("vars = struct(%s);" % ', '.join(recover))

    def stuff_vec(self, vec, start, end, expr, stride):
        """ Stuffing here is 1 indexed, even though in matlab_encoder we stay
            0 indexed.  Hopefully this can be cleaned up!
        """
        if stride == 1:
            yield "%s(%d:%d) = %s;" % (vec, start+1, end, encoder.toMatlab(expr))
        else:
            yield "%s(%d:%d:%d) = %s;" % (vec, start+1, stride, end, encoder.toMatlab(expr))
    
    def stuff_c(self, start, end, expr, stride = 1):
        return self.stuff_vec("c", start, end, expr, stride)

    def stuff_b(self, start, end, expr, stride = 1):
        return self.stuff_vec("b", start, end, expr, stride)

    def stuff_h(self, start, end, expr, stride = 1):
        return self.stuff_vec("h", start, end, expr, stride)

    def stuff_matrix(self, mat, r0, rend, c0, cend, expr, rstride):
        n = (rend - r0) / rstride
        if n > 1 and expr.isscalar: expr = OnesCoeff(n, expr)
        
        yield "%si = [%si; %s];" % (mat, mat, encoder.toMatlab(expr.I(r0, rstride)))
        yield "%sj = [%sj; %s];" % (mat, mat, encoder.toMatlab(expr.J(c0)))
        yield "%sv = [%sv; %s];" % (mat, mat, encoder.toMatlab(expr.V()))

    def stuff_A(self, r0, rend, c0, cend, expr, rstride = 1):
        return self.stuff_matrix("A", r0, rend, c0, cend, expr, rstride)

    def stuff_G(self, r0, rend, c0, cend, expr, rstride = 1):
        return self.stuff_matrix("G", r0, rend, c0, cend, expr, rstride)


    # def visit_Program(self, node):
    #     # check to make sure dimensions are defined
    #     dimensions_defined = map(lambda x: x in node.dimensions, self.args.keys())
    #     if dimensions_defined and all(dimensions_defined):
    #         ast.Dimension.lookup = self.args  # set the dimension lookup table
    #         node.dimensions = []            # empty out the dimension list
    #     else:
    #         raise Exception("MatlabCodegen: Dimensions need to be defined for Matlab.")
    #     super(MatlabCodegen,self).visit_Program(node)
    #     ast.Dimension.lookup = None    # reset the lookup table

    # def visit_Slice(self, node):
    #     self.generic_visit(node)
    #     e = self.expr_stack.pop()
    #     a = {}
    #     for k,v in e.iteritems():
    #         a[k] = v.eval(self.dims).slice(node.begin, node.end)
    # 
    #     self.expr_stack.append(a)
    # 
    # def function_prototype(self):
    #     # maybe put params into a sparse structure?
    #     yield "" #["function result = solve(%s)" % ', '.join(dims + params)]
    # 
    # def function_preamble(self):
    #     yield "%"
    #     yield "% Solves the optimization problem"
    #     yield "%     TODO"
    #     yield "%"
    #     yield "%% dimensions are: %s" % self.dims
    #     yield ""
    # 
    # def function_datastructures(self):
    #     """
    #         varlength
    #             length of x variable (as int)
    #         num_lineqs
    #             number of linear equality constraints (as int)
    #         num_lps
    #             number of linear inequality constraints (as int)
    #         num_conic
    #             number of SOC constraints (as int)
    #         cone_list
    #             list of *tuple* of SOC dimensions; tuple is (num, sz), with num
    #             being the multiplicity of the cone size, e.g. (2, 3) means *two*
    #             3-dimensional cones. it is a tuple of strings
    #     """
    #     def cone_tuple_to_str(x):
    #         num, sz = x
    #         if str(num) == '1':
    #             return "%s" % sz
    #         else:
    #             return "%s*ones(%s,1)" % (sz, num)
    #     cone_list_str = map(cone_tuple_to_str, self.cone_list)
    #     yield "solver.c = zeros(%s,1);" % self.num_vars
    #     yield "solver.h = zeros(%s,1);" % (self.num_conic + self.num_lps)
    #     yield "solver.b = zeros(%s,1);" % self.num_lineqs
    #     yield "solver.G = sparse(%s,%s);" % (self.num_conic + self.num_lps, self.num_vars)
    #     yield "solver.A = sparse(%s,%s);" % (self.num_lineqs, self.num_vars)
    #     yield "solver.dims = struct('l', %s, 'q', [%s], 's', []);" % (self.num_lps, '; '.join(map(str,cone_list_str)))
    # 
    # def function_solve(self):
    #     # if self.cone_size is not None:
    #     #     return ["[cnew, Gnew, hnew, dimsnew,Anew] = prob2fixedcones(solver.c, solver.G, solver.h, solver.dims, solver.A, %s)" % self.cone_size,
    #     #         "result = fillintest(Anew, Gnew, dimsnew);",
    #     #         # "cvx_begin",
    #     #         # "  variable x(%s)" % self.num_vars,
    #     #         # "  variable s(%s)" % (self.num_conic + self.num_lps),
    #     #         # "  minimize (solver.c' * x)",
    #     #         # "  subject to",
    #     #         # "    solver.A * x == solver.b",
    #     #         # "    solver.G * x + s == solver.h",
    #     #         # "    s(1:solver.dims.l) >= 0",
    #     #         # "    ind = solver.dims.l;",
    #     #         # "    for i = 1:length(solver.dims.q),",
    #     #         # "      norm( s(ind+2:ind + solver.dims.q(i)) ) <= s(ind+1)",
    #     #         # "      ind = ind + solver.dims.q(i);",
    #     #         # "    end",
    #     #         # "cvx_end",
    #     #         "[primal_sol dual_sol info] = ecos(cnew, Gnew, hnew, dimsnew,Anew,solver.b);"
    #     #         ""
    #     #     ]
    #     # else:
    #     yield "result = fillintest(solver.A,solver.G,solver.dims);"
    #         # "cvx_begin",
    #         # "  variable x(%s)" % self.num_vars,
    #         # "  variable s(%s)" % (self.num_conic + self.num_lps),
    #         # "  minimize (solver.c' * x)",
    #         # "  subject to",
    #         # "    solver.A * x == solver.b",
    #         # "    solver.G * x + s == solver.h",
    #         # "    s(1:solver.dims.l) >= 0",
    #         # "    ind = solver.dims.l;",
    #         # "    for i = 1:length(solver.dims.q),",
    #         # "      norm( s(ind+2:ind + solver.dims.q(i)) ) <= s(ind+1)",
    #         # "      ind = ind + solver.dims.q(i);",
    #         # "    end",
    #         # "cvx_end",
    #     yield "[primal_sol dual_sol info] = ecos(solver.c,solver.G,solver.h,solver.dims,solver.A,solver.b);"
    #     yield ""
    # 
    # def function_recover(self,keys):
    #     # recover the old variables
    #     recover = (
    #         "%s = x(%s+1:%s);" % (k, self.varstart['_'+k], self.varstart['_'+k]+self.varlength['_'+k])
    #             for k in keys
    #     )
    # 
    #     yield ""
    #     #     "% do the reverse mapping",
    #     #     "%s" % ('\n').join(recover)
    #     # ]
    # 
    # def function_stuff_c(self, start, end, expr):
    #     yield "solver.c(%s+1:%s) = %s;" % (start, end, expr)
    # 
    # def function_stuff_b(self, start, end, expr):
    #     yield "solver.b(%s+1:%s) = %s;" % (start, end, expr)
    # 
    # def function_stuff_h(self, start, end, expr, stride = None):
    #     if stride is not None:
    #         yield "solver.h(%s+1:%s:%s) = %s;" % (start, stride, end, expr)
    #     else:
    #         yield "solver.h(%s+1:%s) = %s;" % (start, end, expr)
    # 
    # def function_stuff_G(self, row_start, row_end, col_start, col_end, expr, row_stride = None):
    #     if row_stride is not None:
    #         yield "solver.G(%s+1:%s:%s, %s+1:%s) = %s;" % (row_start, row_stride, row_end, col_start, col_end, expr)
    #     else:
    #         yield "solver.G(%s+1:%s, %s+1:%s) = %s;" % (row_start, row_end, col_start, col_end, expr)
    # 
    # def function_stuff_A(self, row_start, row_end, col_start, col_end, expr, row_stride = None):
    #     if row_stride is not None:
    #         yield "solver.A(%s+1:%s:%s, %s+1:%s) = %s;" % (row_start, row_stride, row_end, col_start, col_end, expr)
    #     else:
    #         yield "solver.A(%s+1:%s, %s+1:%s) = %s;" % (row_start, row_end, col_start, col_end, expr)
    # 
    # def __create_variable(self, n):
    #     """Creates a new, temporary variable name"""
    #     name = 's' + str(self.new_soc_vars)
    #     self.new_soc_vars += 1
    #     return Variable(name, n)
    # 
    # def __slice(self, node, begin, end):
    #     if isinstance(node, expression.Slice):
    #         new_begin = node.begin + begin
    #         new_end = node.begin + end
    # 
    #         return expression.Slice(node.value, new_begin, new_end, 0)
    # 
    #     if isinstance(node, expression.Mul):
    #         slice_left = self.__slice(node.left, begin, end)
    #         return expression.Mul(slice_left, node.right)
    # 
    #     return expression.Slice(node, begin, end, 0)

