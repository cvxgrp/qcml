from scoop.qc_ast import NodeVisitor, isscalar, RelOp, SOC, SOCProd
from dimensions import Dimension

# define code generation objects
# these are for cvxopt...
# TODO: can change behavior of these by changing __str__ definition
class CodegenExpr(object):
    # operations only occur on objects with the same shape    
    def __add__(self, other):
        if isinstance(self, Constant) and isinstance(other, Constant):
            return Constant(self.value + other.value)
        if isinstance(self, Constant) and self.value == 0:
            return other
        if isinstance(other,Constant) and other.value == 0:
            return self
        if isinstance(self,Eye) and isinstance(other,Eye):
            return Eye(self.n, self.coeff + other.coeff)
        if isinstance(self,Ones) and isinstance(other,Ones) and (self.transpose == other.transpose):
            return Ones(self.n, self.coeff + other.coeff, self.transpose)
        if str(self) == str(other):
            return Constant(2.0) * self
        return Add(self, other)
    
    def __sub__(self, other):
        return self + (-other)
        
    def __neg__(self):
        if isinstance(self, Constant):
            return Constant(-self.value)
        if isinstance(self, Parameter):
            return Parameter("-" + self.value)
        if isinstance(self,Eye):
            return Eye(self.n, -self.coeff)
        if isinstance(self,Negate):
            return self.arg
        if isinstance(self,Ones):
            return Ones(self.n, -self.coeff)
        if isinstance(self,Mul):
            return Mul(-self.left, self.right)
        return Negate(self)
    
    def __mul__(self,other):
        if isinstance(self, Constant) and isinstance(other, Constant):
            return Constant(self.value * other.value)
        if isinstance(self,Constant) and self.value == 1:
            return other
        if isinstance(other,Constant) and other.value == 1:
            return self
        
        if isinstance(self,Eye) and other.isknown and other.isscalar:
            return Eye(self.n, self.coeff * other)
        if isinstance(other,Eye) and self.isknown and self.isscalar:
            return Eye(other.n, other.coeff * self)
            
        if isinstance(self,Eye) and isinstance(other,Eye):
            # (a*I) * (b*I) = (a*b*I)
            return Eye(self.n, self.coeff * other.coeff)
        if isinstance(self,Eye) and isinstance(self.coeff, Constant) and self.coeff.value == 1:
            # I*x = x
            return other
        if isinstance(other,Eye) and isinstance(other.coeff, Constant) and other.coeff.value == 1:
            # x*I = x
            return self  
        if isinstance(self,Eye) and isinstance(self.coeff, Constant) and self.coeff.value == -1:
            return -other
        if isinstance(other,Eye) and isinstance(other.coeff, Constant) and other.coeff.value == -1:
            return -self  
        if isinstance(self,Ones) and self.transpose and isinstance(other,Ones) and not other.transpose:
            # ones^T ones
            return Parameter(self.n) * self.coeff * other.coeff
        if isinstance(self,Ones) and other.isknown and other.isscalar:
            return Ones(self.n, self.coeff*other, self.transpose)
        if isinstance(other,Ones) and self.isknown and self.isscalar:
            return Ones(other.n, other.coeff*self, other.transpose)

        return Mul(self, other)

    def trans(self):
        if isinstance(self, Constant):
            return self
        if isinstance(self, Eye):
            return self
        if isinstance(self, Ones):
            self.transpose = not self.transpose
            return self
            
        return Transpose(self)
    
    def __repr__(self): return str(self)

class Constant(CodegenExpr):
    def __init__(self, value):
        self.value = value
        self.isknown = True
        self.isscalar = True
        
    def __str__(self): return str(self.value)

class Parameter(CodegenExpr):
    def __init__(self, value):
        self.value = value
        self.isknown = True
        self.isscalar = False
    
    def __str__(self): return self.value

class ScalarParameter(Parameter):
    def __init__(self,value):
        super(ScalarParameter, self).__init__(value)
        self.isscalar = True

class Negate(CodegenExpr):
    def __init__(self, arg):
        self.arg = arg
        self.isknown = arg.isknown
        self.isscalar = arg.isscalar
    
    def __str__(self): return "-(%s)" % self.arg

class Eye(CodegenExpr):
    def __init__(self, n, coeff):
        self.n = n
        self.coeff = coeff
        self.isknown = True
        self.isscalar = False
        
    # def __str__(self): return "_o.spmatrix(%s,range(%s),range(%s), tc='d')" % (self.coeff, self.n, self.n)

class Ones(CodegenExpr):
    def __init__(self, n, coeff, transpose = False):
        self.n = n
        self.coeff = coeff
        self.transpose = transpose
        self.isknown = True
        self.isscalar = False
        
    # def __str__(self): 
    #     if self.transpose:
    #         return "_o.matrix(%s,(1,%s), tc='d')" % (self.coeff, self.n)
    #     else:
    #         return "_o.matrix(%s,(%s,1), tc='d')" % (self.coeff, self.n)

class Add(CodegenExpr):
    def __init__(self, left, right):
        self.left = left
        self.right = right
        self.isknown = left.isknown and right.isknown
        self.isscalar = left.isscalar and right.isscalar
    
    def __str__(self): return "%s + %s" % (self.left, self.right)

class Mul(CodegenExpr):
    def __init__(self, left, right):
        self.left = left
        self.right = right
        self.isknown = left.isknown and right.isknown
        self.isscalar = left.isscalar and right.isscalar
    
    def __str__(self): return "%s * %s" % (self.left, self.right)
    

class Transpose(CodegenExpr):
    def __init__(self, arg):
        self.arg = arg
        self.isknown = arg.isknown
        self.isscalar = arg.isscalar
    
    # def __str__(self): return "(%s).trans()" % self.arg


""" Codegen template.

    Actually, this is *not* a code generator.
    
    It is a *python* code generator that executes the code and execs it.
"""
class Codegen(NodeVisitor):
    """ Abstract class. Generates code for solver by walking the problem tree.
    
        Does not depend on any known data types. 
        
        It generates code by calling the following sequence of functions:
        
        function_prototype
            creates the prototype for the function
        function_preamble
            any global setup, etc. code
        function_datastructures
            specifically, set up the data structures containing the problem data
            c, G, h, A, b, and dims

        function_solve
            the specific call to the solver
        function_recover
            how to recover the solution from the solver
            
        Before calling "function_solve", we make repeated calls to
        
        function_stuff_c, function_stuff_h, function_stuff_b
        function_stuff_G, function_stuff_A
        
        These functions stuff the data into the datastructures set up in 
        function_datastructures.
    """
    offset = 4*' '
    constraint = 8*' '
     # the lookup is for adding comments
     # we can actually pass the lookup from the rewriter to the code generator
     # however, i am not doing that at the moment
    def __init__(self):
        """ Walks the tree and creates the data structures.
            Calls the solver.
        """
        self.prog = []
        self.varlength = {}
        self.varstart = {}
        self.expr_stack = []
        self.num_lineqs = Dimension(0)
        self.num_lps = Dimension(0)
        self.num_conic = Dimension(0)
        self.comment = '#'
    
    def codegen(self):
        exec '\n'.join(self.prog) in locals()
        return solve
    
    def prettyprint(self,lineno=False):
        """ Pretty prints the source code, possibly with line numbers
        """
        if lineno:
            print '\n'.join( map(lambda x: "%4s    %s" % (x[0],x[1]), zip( range(1,len(self.prog)+1), self.prog ))  )
        else:
            print '\n'.join(self.prog)
    
    def function_prototype(self, dims, params): pass
    
    def function_preamble(self): pass
    
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
        pass
    
    def function_solve(self): pass
    
    def function_recover(self,keys): pass
    
    def function_stuff_c(self, start, end, expr): pass
    
    def function_stuff_b(self, start, end, expr): pass
    
    def function_stuff_h(self, start, end, expr, stride = None): pass
    
    def function_stuff_G(self, row_start, row_end, col_start, col_end, expr, row_stride = None): pass
    
    def function_stuff_A(self, row_start, row_end, col_start, col_end, expr, row_stride = None): pass
    
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
                cone_list.append( ("1", str(cone_len)) )
            if isinstance(c, SOCProd):     
                cone_list.append( (c.shape.size_str(), str(len(c.arglist) + 1)) )
                num_conic += Dimension(None, {c.shape.size_str(): len(c.arglist) + 1})
        # TODO: the above code can be done by walking the tree. we just have to ensure that the code we emit when walking the tree is buffered somewhere else before printing
            
        self.prog = self.function_prototype(dims, params)
        self.prog += map(lambda x: self.offset + x, self.function_preamble())
        self.prog += map(lambda x: self.offset + x, self.function_datastructures(varlength, num_lineqs, num_lps, num_conic, cone_list))
        
        self.generic_visit(node)
        
        self.prog += map(lambda x: self.offset + x, 
            self.function_solve() + self.function_recover(node.variables.keys()))
            
        
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
        self.prog.append(self.offset + "%s stuffing the objective vector" % self.comment)
        
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
                    self.function_stuff_c(start, start+length, objective_c))

        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack
        self.prog.append("")
    
    def visit_RelOp(self, node):
        # TODO: in rewriter, force relop to be a - b <= 0
        # and a - b == 0
        self.prog.append(self.offset + "%s for the constraint %s" % (self.comment, node))
        
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
                    self.prog.append(self.offset + self.function_stuff_b(start, self.num_lineqs, -v))
                else:
                    self.prog.append(self.offset + self.function_stuff_h(start, self.num_lps, -v))
            else:
                xstart = self.varstart[k]
                xend = xstart + self.varlength[k]
                if node.op == '==':
                    string = self.function_stuff_A(start, self.num_lineqs, xstart, xend, v)
                    self.prog.append(self.offset + string)
                else:
                    string = self.function_stuff_G(start, self.num_lps, xstart, xend, v)
                    self.prog.append(self.offset + string)
        
        self.prog.append("")
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack
    
 
    def visit_SOC(self, node):
        self.prog.append( self.offset + "%s for the SOC constraint %s" % (self.comment, node) )
        
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
                        self.function_stuff_h(conestart, coneend, v))
                else:
                    xstart = self.varstart[k]
                    xend = xstart + self.varlength[k]
                    self.prog.append( self.offset + \
                        self.function_stuff_G(conestart, coneend, xstart, xend, -v))
        
        self.prog.append("")
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack

    def visit_SOCProd(self, node):
        self.prog.append( self.offset + "%s for the SOC product constraint %s" % (self.comment, node) )
        
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
                        self.function_stuff_h(conestart, coneend, v, stride))
                else:
                    xstart = self.varstart[k]
                    xend = xstart + self.varlength[k]
                    self.prog.append( self.offset + \
                        self.function_stuff_G(conestart, coneend, xstart, xend, -v, stride))
        
        self.prog.append("")
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack