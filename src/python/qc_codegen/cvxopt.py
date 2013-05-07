from scoop.qc_ast import NodeVisitor, isscalar, RelOp, SOC, SOCProd
from codegen import Constant, ScalarParameter, Parameter, Eye, Ones
from dimensions import Dimension


class CVXOPTCodegen(NodeVisitor):
    """ Generates code for CVXOPT
        
        Actually, need some way to insert different code generation..
        Or, make this an easy to repeat template (like the one above)
    """
    offset = 4*' '
    constraint = 8*' '
     # the lookup is for adding comments
     # we can actually pass the lookup from the rewriter to the code generator
     # however, i am not doing that at the moment
    def __init__(self, comments):
        """ Walks the tree and creates the data structures.
            Calls the solver.
            
            Another possibility is to walk the tree and create a Python AST and
            that would "def" a function which can be used as a solver.
            
            Probably build the entire function as a python string... Then call
            ast.compile
            
            and then use "exec".
            
            Can guarantee "safety" since no code will run unless it passes our
            lexer and parser. And we can only do a couple things: stuff matrices,
            call CVXOPT, etc.
            
            You have to modify *this* source code to make a malicious program.
        """
        self.prog = []
        self.varlength = {}
        self.varstart = {}
        self.expr_stack = []
        self.num_lineqs = Dimension(0)
        self.num_lps = Dimension(0)
        self.num_conic = Dimension(0)
    
    def codegen(self):
        return self.solve
    
    def prettyprint(self,lineno=False):
        """ Pretty prints the source code, possibly with line numbers
        """
        if lineno:
            print '\n'.join( map(lambda x: "%4s    %s" % (x[0],x[1]), zip( range(1,len(self.prog)), self.prog ))  )
        else:
            print '\n'.join(self.prog)

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
            "_sol = _o.solvers.conelp(_c, _G, _h, _dims, _A, _b)",
            "",
            "# do the reverse mapping",
            "return {'info': _sol, %s }" % (', '.join(recover))
        ])
    
        exec '\n'.join(self.prog) in locals()
        self.solve = solve
        
    def visit_Variable(self, node):
        n = node.shape.size_str()
        if node.value in self.orig_varnames:
            k = '_' + node.value
        else:
            k = node.value
            
        if n == "1":
            lineq = {k: Constant(1)}
        else:
            lineq = {k: Eye(n,Constant(1))}
        
        self.expr_stack.append(lineq)
    
    def visit_Parameter(self, node):
        if isscalar(node):
            self.expr_stack.append({'1':ScalarParameter(node.value)})
        else:
            self.expr_stack.append({'1':Parameter(node.value)})
            
    
    def visit_Constant(self, node):
        self.expr_stack.append({'1':Constant(node.value)})
        
    def visit_Transpose(self, node):
        self.generic_visit(node)
        
        arg = self.expr_stack.pop()
        
        for k,v in arg.iteritems():
            arg[k] = arg[k].trans()
        
        self.expr_stack.append(arg)
    
    def visit_Mul(self, node):
        # at this stage, lineq stack guaranteed to contain a constant / parameter
        if not node.left.isknown:
            raise SyntaxError("unknown error occurred in parsing stage. multiply has non-const and non-param lefthand side.")
        
        self.generic_visit(node)
        
        
        right = self.expr_stack.pop()
        left = self.expr_stack.pop()
        
        # left should always be known constant
        coeff = left['1']
        
        for k in right.keys():
            # does not optimize gamma*matrix(1, (1,n))
            right[k] =  coeff * right[k]
        
        self.expr_stack.append(right)
    
    def visit_Sum(self, node):
        self.generic_visit(node)
        
        arg = self.expr_stack.pop()
        
        for k in arg.keys():
            arg[k] =  Ones(node.arg.shape.size_str(), Constant(1), True) * arg[k]
            
        self.expr_stack.append(arg)
    
    def visit_Add(self, node):
        self.generic_visit(node)
        
        right = self.expr_stack.pop()
        left = self.expr_stack.pop()
        
        for k in right.keys():
            if left.get(k, None) is not None:
                left[k] = left[k] + right[k]
            else:
                left[k] = right[k]
    
        self.expr_stack.append(left)
    
    def visit_Negate(self, node):
        self.generic_visit(node)
        
        arg = self.expr_stack.pop()
        for k in arg.keys():
            arg[k] = -arg[k]
        
        self.expr_stack.append(arg)
        
    
    def visit_Objective(self, node):
        self.prog.append("")
        self.prog.append(self.offset + "# stuffing the objective vector")
        
        self.generic_visit(node)
        
        obj = self.expr_stack.pop()
        if not node.sense == 'find':
            for k,v in obj.iteritems():
                # ignore constants
                if k == '1':
                    continue
                start = self.varstart[k]
                length = self.varlength[k]
                if node.sense == 'minimize':
                    objective_c = v.trans()
                elif node.sense == 'maximize':
                    objective_c = (-v).trans()
                self.prog.append(self.offset + \
                    "_c[%s:%s] = %s" % (
                        start,
                        start + length,
                        objective_c
                    ))
        
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack
        self.prog.append("")
    
    def visit_RelOp(self, node):
        # TODO: in rewriter, force relop to be a - b <= 0
        # and a - b == 0
        self.prog.append(self.offset + "# for the constraint %s" % (node))
        
        if node.op == '==':
            start = self.num_lineqs
            self.num_lineqs += Dimension(node.shape.size_str())
        else:            
            start = self.num_lps            
            self.num_lps += Dimension(node.shape.size_str())            

        self.generic_visit(node)
        
        right = self.expr_stack.pop()
        left = self.expr_stack.pop()
        
        # right should be all 0's
        assert (right['1'].value == 0), "Expected '0' on the righthand side, but got %s." % right['1']
        
        # copy into the appropriate block
        for k,v in left.iteritems():
            if k == '1':
                if node.op == '==':
                    self.prog.append(self.offset + "_b[%s:%s] = %s" % (start, self.num_lineqs, -v))
                else:
                    self.prog.append(self.offset + "_h[%s:%s] = %s" % (start, self.num_lps, -v))
            else:
                xstart = self.varstart[k]
                xend = xstart + self.varlength[k]
                if node.op == '==':
                    self.prog.append(self.offset + "_A[%s:%s, %s:%s] = %s" % (start, self.num_lineqs, xstart, xend, v))
                else:
                    self.prog.append(self.offset + "_G[%s:%s, %s:%s] = %s" % (start, self.num_lps, xstart, xend, v))
        
        self.prog.append("")
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack
    
 
    def visit_SOC(self, node):
        # TODO!!!!!
        self.prog.append( self.offset + "# for the SOC constraint %s" % (node) )
        
        # we assume linear constraints have already been handled
        start = [self.num_lps + self.num_conic]
        
        start += [start[-1] + Dimension(node.right.shape.size_str())]
        self.num_conic += Dimension(node.right.shape.size_str())
        for e in node.left:
            start += [start[-1] + Dimension(e.shape.size_str())]
            self.num_conic += Dimension(e.shape.size_str())
        
        self.generic_visit(node)
        
        # copy into the appropriate block
        count = 0
        while self.expr_stack:
            e = self.expr_stack.pop()
            conestart = start[count]
            coneend = start[count+1]
            count += 1
            for k,v in e.iteritems():
                if k == '1':
                    self.prog.append( self.offset + \
                        "_h[%s:%s] = %s" % (conestart, coneend, v))
                else:
                    xstart = self.varstart[k]
                    xend = xstart + self.varlength[k]
                    self.prog.append( self.offset + \
                        "_G[%s:%s, %s:%s] = %s" % (conestart, coneend, xstart, xend, -v))
        
        self.prog.append("")
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack

    def visit_SOCProd(self, node):
        self.prog.append( self.offset + "# for the SOC product constraint %s" % (node) )
        
        # we assume linear constraints have already been handled
        start = self.num_lps + self.num_conic
        stride = len(node.arglist) + 1
        self.num_conic += Dimension(None, {node.shape.size_str(): stride})
        
        self.generic_visit(node)
        
        # copy into the appropriate block
        count = 0
        coneend = self.num_conic + self.num_lps
        
        while self.expr_stack:
            e = self.expr_stack.pop()
            conestart = start + Dimension(count)
            count += 1
            for k,v in e.iteritems():
                if k == '1':
                    self.prog.append( self.offset + \
                        "_h[%s:%s:%s] = %s" % (conestart, coneend, stride, v))
                else:
                    xstart = self.varstart[k]
                    xend = xstart + self.varlength[k]
                    self.prog.append( self.offset + \
                        "_G[%s:%s:%s, %s:%s] = %s" % (conestart, coneend, stride, xstart, xend, -v))
        
        self.prog.append("")
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack