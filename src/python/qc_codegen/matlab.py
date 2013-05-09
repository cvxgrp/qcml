#from scoop.qc_ast import NodeVisitor, isscalar, RelOp, SOC, SOCProd
from codegen import Eye, Ones, Transpose, Codegen, Constant
from dimensions import Dimension

def matlab_eye(self):
    if isinstance(self.coeff, Constant) and self.coeff.value == 1:
        return "eye(%s,%s)" % (self.n, self.n)
    else:
        return "%s*eye(%s,%s)" % (self.coeff, self.n, self.n)
        
def matlab_ones(self):
    if self.transpose:
        sz = "1,%s" % self.n
    else:
        sz = "%s,1" % self.n
    
    if isinstance(self.coeff, Constant) and self.coeff.value == 1:
        return "ones(%s)" % sz
    else:
        return "%s*ones(%s)" % (self.coeff, sz)

def matlab_trans(self):
    return "(%s)'" % self.arg

class MatlabCodegen(Codegen):
    def __init__(self):
        super(MatlabCodegen,self).__init__()
        Ones.__str__ = matlab_ones
        Eye.__str__ = matlab_eye
        Transpose.__str__ = matlab_trans
        self.comment = '%'
        
    def function_prototype(self, dims, params):
        # maybe put params into a sparse structure?
        return ["function result = solve(%s)" % ', '.join(dims + params)]
    
    def function_preamble(self):
        return [
        "%",
        "% Solves the optimization problem",
        "%     TODO",
        "%",
        ""]
    
    def function_datastructures(self, varlength, num_lineqs, num_lps, num_conic, cone_list):
        """
            varlength
                length of x variable (as Dimension)
            num_lineqs
                number of linear equality constraints (as Dimension)
            num_lps
                number of linear inequality constraints (as Dimension)
            num_conic
                number of SOC constraints (as Dimension)
            cone_list
                list of *tuple* of SOC dimensions; tuple is (num, sz), with num
                being the multiplicity of the cone size, e.g. (2, 3) means *two*
                3-dimensional cones. it is a tuple of strings
        """
        def cone_tuple_to_str(x):
            num, sz = x
            if num == '1':
                return "%s" % sz
            else:
                return "%s*ones(%s,1)" % (sz, num)
        cone_list_str = map(cone_tuple_to_str, cone_list)
        return [
        "solver.c = zeros(%s,1);" % varlength,
        "solver.h = zeros(%s,1);" % (num_conic + num_lps),
        "solver.b = zeros(%s,1);" % num_lineqs,
        "solver.G = sparse(%s,%s);" % (num_conic + num_lps, varlength),
        "solver.A = sparse(%s,%s);" % (num_lineqs, varlength),
        "solver.dims = struct('l', %s, 'q', [%s], 's', []);" % (num_lps, '; '.join(map(str,cone_list_str)))]
    
    def function_solve(self):
        return ["result = solver; % don't do anything"]
    
    def function_recover(self,keys):
        # # recover the old variables
        # recover = [
        #     "'%s' : _sol['x'][%s:%s]" % (k, self.varstart['_'+k], self.varstart['_'+k]+self.varlength['_'+k]) 
        #         for k in keys
        # ]
        # 
        return [""]
            # "# do the reverse mapping",
            # "return {'info': _sol, %s }" % (', '.join(recover))
    
    def function_stuff_c(self, start, end, expr):
        return "solver.c(%s+1:%s) = %s;" % (start, end, expr)
    
    def function_stuff_b(self, start, end, expr):
        return "solver.b(%s+1:%s) = %s;" % (start, end, expr)
    
    def function_stuff_h(self, start, end, expr, stride = None):
        if stride is not None:
            return "solver.h(%s+1:%s:%s) = %s;" % (start, stride, end, expr)
        else:
            return "solver.h(%s+1:%s) = %s;" % (start, end, expr)
    
    def function_stuff_G(self, row_start, row_end, col_start, col_end, expr, row_stride = None):
        if row_stride is not None:
            return "solver.G(%s+1:%s:%s, %s+1:%s) = %s;" % (row_start, row_stride, row_end, col_start, col_end, expr)
        else:
            return "solver.G(%s+1:%s, %s+1:%s) = %s;" % (row_start, row_end, col_start, col_end, expr)
    
    def function_stuff_A(self, row_start, row_end, col_start, col_end, expr, row_stride = None):
        if row_stride is not None:
            return "solver.A(%s+1:%s:%s, %s:%s) = %s;" % (row_start, row_stride, row_end, col_start, col_end, expr)
        else:
            return "solver.A(%s+1:%s, %s:%s) = %s;" % (row_start, row_end, col_start, col_end, expr)