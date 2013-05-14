#from scoop.qc_ast import NodeVisitor, isscalar, RelOp, SOC, SOCProd
from codegen import PythonCodegen, Ones, Eye, Transpose
from dimensions import Dimension

def cvxopt_eye(self):
    return "_o.spmatrix(%s,range(%s),range(%s), tc='d')" % (self.coeff, self.n, self.n)

def cvxopt_ones(self):
    if self.transpose:
        return "_o.matrix(%s,(1,%s), tc='d')" % (self.coeff, self.n)
    else:
        return "_o.matrix(%s,(%s,1), tc='d')" % (self.coeff, self.n)

def cvxopt_trans(self):
    return "(%s).trans()" % self.arg
    

class CVXOPTCodegen(PythonCodegen):
    def __init__(self):
        Ones.__str__ = cvxopt_ones
        Eye.__str__ = cvxopt_eye
        Transpose.__str__ = cvxopt_trans
        super(CVXOPTCodegen,self).__init__()
        
    def function_prototype(self, dims, params):
        return ["def solve(%s):" % ', '.join(dims + params)]
    
    def function_preamble(self):
        return [
        "import cvxopt as _o",
        "import cvxopt.solvers",
        ""]
    
    def function_datastructures(self):
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
            if str(num) == '1':
                return "[%s]" % sz
            else:
                return "%s*[%s]" % (num, sz)
        cone_list_str = map(cone_tuple_to_str, self.cone_list)
        return [
        "_c = _o.matrix(0, (%s,1), tc='d')" % self.num_vars,
        "_h = _o.matrix(0, (%s,1), tc='d')" % (self.num_conic + self.num_lps),
        "_b = _o.matrix(0, (%s,1), tc='d')" % self.num_lineqs,
        "_G = _o.spmatrix([], [], [], (%s,%s), tc='d')" % (self.num_conic + self.num_lps, self.num_vars),
        "_A = _o.spmatrix([], [], [], (%s,%s), tc='d')" % (self.num_lineqs, self.num_vars),
        "_dims = {'l': %s, 'q': %s, 's': []}" % (self.num_lps, '+'.join(map(str,cone_list_str)))]
    
    def function_solve(self):
        return ["_sol = _o.solvers.conelp(_c, _G, _h, _dims, _A, _b)"]
    
    def function_recover(self,keys):
        # recover the old variables
        recover = [
            "'%s' : _sol['x'][%s:%s]" % (k, self.varstart['_'+k], self.varstart['_'+k]+self.varlength['_'+k]) 
                for k in keys
        ]
        
        return [
            "# do the reverse mapping",
            "return {'info': _sol, %s }" % (', '.join(recover))
        ]
    
    def function_stuff_c(self, start, end, expr):
        return "_c[%s:%s] = %s" % (start, end, expr)
    
    def function_stuff_b(self, start, end, expr):
        return "_b[%s:%s] = %s" % (start, end, expr)
    
    def function_stuff_h(self, start, end, expr, stride = None):
        if stride is not None:
            return "_h[%s:%s:%s] = %s" % (start, end, stride, expr)
        else:
            return "_h[%s:%s] = %s" % (start, end, expr)
    
    def function_stuff_G(self, row_start, row_end, col_start, col_end, expr, row_stride = None):
        if row_stride is not None:
            return "_G[%s:%s:%s, %s:%s] = %s" % (row_start, row_end, row_stride, col_start, col_end, expr)
        else:
            return "_G[%s:%s, %s:%s] = %s" % (row_start, row_end, col_start, col_end, expr)
    
    def function_stuff_A(self, row_start, row_end, col_start, col_end, expr, row_stride = None):
        if row_stride is not None:
            return "_A[%s:%s:%s, %s:%s] = %s" % (row_start, row_end, row_stride, col_start, col_end, expr)
        else:
            return "_A[%s:%s, %s:%s] = %s" % (row_start, row_end, col_start, col_end, expr)