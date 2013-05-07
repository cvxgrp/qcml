from scoop.qc_ast import NodeVisitor, isscalar
from codegen import Constant, ScalarParameter, Parameter, Eye, Ones


class CVXOPTCodegen(NodeVisitor):
    """ Generates code for CVXOPT
        
        Actually, need some way to insert different code generation..
        Or, make this an easy to repeat template (like the one above)
    """
    offset = 2*' '
    constraint = 4*' '
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
        self.prog = ""
        self.varlength = {}
        self.varstart = {}
        self.expr_stack = []
    
    def codegen(self):
        return self.solve

    def visit_Program(self, node):
        dims = map(str, node.dimensions)
        params = map(str, node.parameters)
    
        # keep track of original variables
        self.orig_varnames = set(node.variables.keys())
    
        # create variable ordering
        # "_" variables are original variables
        self.varlength = [('_' + k,v.shape.size_str()) for k,v in node.variables.items()]
        self.varlength += [(k,v.shape.size_str()) for k,v in node.new_variables.items()]
    
        self.varstart = []
        start = "0"
        for k,v in self.varlength:
            self.varstart.append( (k, start) )
            start = start + "+" + v
    
        # varstart contains the start indicies as strings
        self.varstart = dict(self.varstart)
        # varlength contains the vector lengths as strings
        self.varlength = dict(self.varlength)
    
        self.prog = \
"""
def solve(%s):
  \"""
  Default solves scalar problem
       find x
       s.t. x <= 0
  \"""
  import cvxopt as _o
  import cvxopt.solvers

  _c = _o.matrix(0, (%s,1), tc='d')
  _G = _o.matrix(1, (1,%s), tc='d')
  _h = _o.matrix(1, (1,1), tc='d')
  _dims = {'l':1, 'q':[], 's':[]}
  _A = _o.matrix(1,(0,%s), tc='d')
  _b = _o.matrix(1,(0,1), tc='d')
""" % (', '.join(dims + params), start, start, start)
    
        self.generic_visit(node)
        self.prog += self.offset + "return _o.solvers.conelp(_c, _G, _h, _dims, _A, _b)\n"
        #self.prog += self.offset + "return sol\n"
    
        print self.varlength
        print self.varstart
        print self.prog
        #self.code = compile(self.prog, '', 'exec')
    
        exec self.prog in locals()
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
        self.prog += "\n"
        self.prog += self.offset + "# stuffing the objective vector\n"
        
        self.generic_visit(node)
        
        obj = self.expr_stack.pop()
        for k,v in obj.iteritems():
            # ignore constants
            if k == '1':
                continue
            start = self.varstart[k]
            length = self.varlength[k]
            self.prog += self.offset + \
                "_c[%s:%s] = %s\n" % (
                    start,
                    start + "+" + length,
                    v.trans()
                )
        
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack
        self.prog += "\n"
    
    def visit_RelOp(self, node):
        # TODO: in rewriter, force relop to be a - b <= 0
        # and a - b == 0
        self.generic_visit(node)
        right = self.expr_stack.pop()
        left = self.expr_stack.pop()
        
        # right should be all 0's
        print left
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack
    
 
    def visit_SOC(self, node):
        self.generic_visit(node)
        print self.expr_stack
        self.expr_stack = []

    def visit_SOCProd(self, node):
        self.generic_visit(node)
        print self.expr_stack
        self.expr_stack = []