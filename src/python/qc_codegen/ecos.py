from cvxopt import CVXOPTCodegen
from dimensions import Dimension
from scoop.qc_ast import RelOp, SOC, SOCProd


class ECOSCodegen(CVXOPTCodegen):
    """ Generates code for ECOS
    """
    def visit_Program(self, node):
        dims = map(str, node.dimensions)    # also includes unused dimensions
        params = map(str, node.parameters)
    
        # keep track of original variables
        self.orig_varnames = set(node.variables.keys())
    
        # create variable ordering
        # "_" variables are original variables
        self.varlength = [('_' + k,Dimension(v.shape.size_str())) for k,v in node.variables.items()]
        self.varlength += [(k,Dimension(v.shape.size_str())) for k,v in node.new_variables.items()]
    
        self.varstart = []
        start = Dimension(0)
        for k,v in self.varlength:
            self.varstart.append( (k, start) )
            start += v
    
        # varstart contains the start indicies as strings
        self.varstart = dict(self.varstart)
        # varlength contains the vector lengths as strings
        self.varlength = dict(self.varlength)
        
        # full length of optimization variable
        varlength = start
        
        # constraints, local copy to instantiate sparse matrices
        num_lineqs = Dimension(0)
        num_lps = Dimension(0)
        cone_list = []
        num_conic = Dimension(0)
        for c in node.constraints:
            if isinstance(c, RelOp):
                if c.op == '==':
                    num_lineqs += Dimension(c.shape.size_str())
                else:
                    num_lps += Dimension(c.shape.size_str())
            if isinstance(c, SOC):
                cone_len = Dimension(c.right.shape.size_str())
                for e in c.left:
                    cone_len += Dimension(e.shape.size_str())
                    
                num_conic += cone_len
                cone_list.append("[%s]" % cone_len)
            if isinstance(c, SOCProd):     
                cone_list.append("%s*[%s]" % (c.shape.size_str(), len(c.arglist) + 1))
                num_conic += Dimension(None, {c.shape.size_str(): len(c.arglist) + 1})
            
        self.prog = ["def solve(%s):" % ', '.join(dims + params)]
        self.prog += map(lambda x: self.offset + x, [
        "\"\"\"",
        "Default solves scalar problem",
        "     find x",
        "     s.t. x <= 0",
        "\"\"\"",
        "import cvxopt as _o",
        "import cvxopt.solvers",
        "import ecos"
        "",
        "_c = _o.matrix(0, (%s,1), tc='d')" % varlength,
        "_h = _o.matrix(0, (%s,1), tc='d')" % (num_conic + num_lps),
        "_b = _o.matrix(0, (%s,1), tc='d')" % num_lineqs,
        "_G = _o.spmatrix([], [], [], (%s,%s), tc='d')" % (num_conic + num_lps, varlength),
        "_A = _o.spmatrix([], [], [], (%s,%s), tc='d')" % (num_lineqs, varlength),
        "_dims = {'l': %s, 'q': %s, 's': []}" % (num_lps, '+'.join(map(str,cone_list))),
        ])
        
        self.generic_visit(node)
        
        # for debugging
        # self.prog += map(lambda x: self.offset + x, [
        #     "print _G",
        #     "print _h"
        # ])
        
        # recover the old variables
        recover = [
            "'%s' : _sol['x'][%s:%s]" % (k, self.varstart['_'+k], self.varstart['_'+k]+self.varlength['_'+k]) 
                for k in node.variables.keys()
        ]
        
        self.prog += map(lambda x: self.offset + x, [
            "_sol = ecos.ecos(_c, _G, _h, _dims, _A, _b)",
            "",
            "# do the reverse mapping",
            "return {'info': _sol, %s }" % (', '.join(recover))
        ])
    
        exec '\n'.join(self.prog) in locals()
        self.solve = solve
  