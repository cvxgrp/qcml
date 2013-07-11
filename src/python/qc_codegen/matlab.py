#from scoop.qc_ast import NodeVisitor, isscalar, RelOp, SOC, SOCProd

from scoop.qc_ast import Variable, Vector, Scalar
import scoop.qc_ast as ast
from codegen import Eye, Ones, Transpose, Slice, Codegen, Constant, Parameter

def matlab_eye(self):
    if isinstance(self.coeff, Constant) and self.coeff.value == 1:
        return "speye(%s,%s)" % (self.n, self.n)
    else:
        return "%s*speye(%s,%s)" % (self.coeff, self.n, self.n)
        
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

def matlab_slice(self):
    if isinstance(self.arg, Parameter):
        if self.transpose:
            return "%s(:,%s+1:%s)'" % (self.arg, self.begin, self.end)
        else:
            return "%s(%s+1:%s,:)" % (self.arg, self.begin, self.end)
    elif isinstance(self.arg, Eye):
        h = self.end - self.begin
        n = int(str(self.arg.n))
        start = self.begin - 1
        end = n - self.end
        return "sparse(1:%s,%s+1:%s,%s, %s,%s)" % (h, self.begin, self.end, self.arg.coeff, h, self.arg.n)
    else:
        print type(self.arg)
        raise Exception("Slice didn't do what I thought it would....")

class MatlabCodegen(Codegen):
    def __init__(self, cone_size=None, **kwargs):
        """
            cone_size
                fixed size of SOC cone. must be 3 or greater
            
            kwargs
                dictionary of problem dimensions
        """
        super(MatlabCodegen,self).__init__()
        
        Ones.__str__ = matlab_ones
        Eye.__str__ = matlab_eye
        Transpose.__str__ = matlab_trans
        Slice.__str__ = matlab_slice
        
        self.comment = '%'
        self.args = kwargs
        if cone_size is not None:
            self.cone_size = max(3,cone_size)
        else:
            self.cone_size = None
        
        self.new_soc_vars = 0
        
    def visit_Program(self, node):
        # check to make sure dimensions are defined
        dimensions_defined = map(lambda x: x in node.dimensions, self.args.keys())
        if dimensions_defined and all(dimensions_defined):
            ast.Dimension.lookup = self.args  # set the dimension lookup table
            node.dimensions = []            # empty out the dimension list
        else:
            raise Exception("MatlabCodegen: Dimensions need to be defined for Matlab.")
        super(MatlabCodegen,self).visit_Program(node)
        ast.Dimension.lookup = None    # reset the lookup table
        
    def visit_Slice(self, node):
        self.generic_visit(node)
        e = self.expr_stack.pop()
        a = {}
        for k,v in e.iteritems():
            a[k] = v.slice(node.begin, node.end)
                
        self.expr_stack.append(a)
        
    def function_prototype(self, dims, params):
        # maybe put params into a sparse structure?
        return [""] #["function result = solve(%s)" % ', '.join(dims + params)]
    
    def function_preamble(self):
        return [
        "%",
        "% Solves the optimization problem",
        "%     TODO",
        "%",
        "%% dimensions are: %s" % self.args,
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
                return "%s" % sz
            else:
                return "%s*ones(%s,1)" % (sz, num)
        cone_list_str = map(cone_tuple_to_str, self.cone_list)
        return [
        "solver.c = zeros(%s,1);" % self.num_vars,
        "solver.h = zeros(%s,1);" % (self.num_conic + self.num_lps),
        "solver.b = zeros(%s,1);" % self.num_lineqs,
        "solver.G = sparse(%s,%s);" % (self.num_conic + self.num_lps, self.num_vars),
        "solver.A = sparse(%s,%s);" % (self.num_lineqs, self.num_vars),
        "solver.dims = struct('l', %s, 'q', [%s], 's', []);" % (self.num_lps, '; '.join(map(str,cone_list_str)))]
    
    def function_solve(self):
        # if self.cone_size is not None:
        #     return ["[cnew, Gnew, hnew, dimsnew,Anew] = prob2fixedcones(solver.c, solver.G, solver.h, solver.dims, solver.A, %s)" % self.cone_size,
        #         "result = fillintest(Anew, Gnew, dimsnew);",
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
        #         "[primal_sol dual_sol info] = ecos(cnew, Gnew, hnew, dimsnew,Anew,solver.b);"
        #         ""
        #     ]
        # else:
        return ["result = fillintest(solver.A,solver.G,solver.dims);",
            # "cvx_begin",
            # "  variable x(%s)" % self.num_vars,
            # "  variable s(%s)" % (self.num_conic + self.num_lps),
            # "  minimize (solver.c' * x)",
            # "  subject to",
            # "    solver.A * x == solver.b",
            # "    solver.G * x + s == solver.h",
            # "    s(1:solver.dims.l) >= 0",
            # "    ind = solver.dims.l;",
            # "    for i = 1:length(solver.dims.q),",
            # "      norm( s(ind+2:ind + solver.dims.q(i)) ) <= s(ind+1)",
            # "      ind = ind + solver.dims.q(i);",
            # "    end",
            # "cvx_end",
            "[primal_sol dual_sol info] = ecos(solver.c,solver.G,solver.h,solver.dims,solver.A,solver.b);"
            ""
        ]
    
    def function_recover(self,keys):
        # recover the old variables
        recover = [
            "%s = x(%s+1:%s);" % (k, self.varstart['_'+k], self.varstart['_'+k]+self.varlength['_'+k]) 
                for k in keys
        ]
        
        return [""]
        #     "% do the reverse mapping",
        #     "%s" % ('\n').join(recover)
        # ]
    
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
            return "solver.A(%s+1:%s:%s, %s+1:%s) = %s;" % (row_start, row_stride, row_end, col_start, col_end, expr)
        else:
            return "solver.A(%s+1:%s, %s+1:%s) = %s;" % (row_start, row_end, col_start, col_end, expr)
    
    def __create_variable(self, n):
        """Creates a new, temporary variable name"""
        name = 's' + str(self.new_soc_vars)
        self.new_soc_vars += 1
        return Variable(name, n)
    
    def __slice(self, node, begin, end):
        if isinstance(node, ast.Slice): 
            new_begin = node.begin + begin
            new_end = node.begin + end
            
            return ast.Slice(node.value, new_begin, new_end, 0)
        
        if isinstance(node, ast.Mul):
            slice_left = self.__slice(node.left, begin, end)
            return ast.Mul(slice_left, node.right)
        
        return ast.Slice(node, begin, end, 0)
            
    def visit_SOC(self, node):
        if self.cone_size is not None:
            # look at the size of the SOC
            cone_length = 1
            for e in node.left:
                dim = ast.Dimension(e.shape.size_str())
                # convert dimension to integer
                cone_length += int(str(dim))
            
            while cone_length > self.cone_size:
                # maximum number of elements on the lhs
                max_lhs = self.cone_size - 1
                
                # collect the new arguments
                new_args = []
                old_args = []
                cum = 0
                create_new = True
                for e in node.left:
                    if create_new:
                        dim = int(str( ast.Dimension(e.shape.size_str()) ))
                        # if the dimension of the current expression doesn't
                        # exceed the max allowable, just push onto argument stack
                        if cum + dim <= max_lhs:
                            new_args.append(e)
                        else:
                            # if it exceeds, only push the slice up to max_lhs
                            new_args.append(self.__slice(e, 0, max_lhs - cum))
                            # save the rest of the expression for another cone
                            old_args.append(self.__slice(e, max_lhs - cum, dim))
                    
                        if cum + dim >= max_lhs:
                            create_new = False                        
                    else:
                        # just push into the old args
                        old_args.append(e)
                    cum += dim
                    
                # create a new variable
                new_var = self.__create_variable(Scalar())                        

                # now add to varlength, varstart, and num_vars
                self.varlength[new_var.value] = ast.Dimension(1)
                self.varstart[new_var.value] = self.num_vars
                self.num_vars += ast.Dimension(1)
                
                # process the new cone, which has the right size
                super(MatlabCodegen,self).visit_SOC(ast.SOC(new_var, new_args))
                
                # process the old cone
                old_args.append(new_var)
            
                node.left = old_args
                cone_length -= (max_lhs - 1) # the extra "1" is the rhs
                
            if cone_length < self.cone_size:
                # create a new variable and append to the node
                new_length = self.cone_size - cone_length
                new_var = self.__create_variable(Vector(new_length))
                node.left.append(new_var)
                
                # now add to varlength, varstart, and num_vars
                self.varlength[new_var.value] = ast.Dimension(new_length)
                self.varstart[new_var.value] = self.num_vars
                self.num_vars += ast.Dimension(new_length)
                        
        super(MatlabCodegen,self).visit_SOC(node)
        
    def visit_SOCProd(self, node):
        if self.cone_size is not None:
            # look at the size of the SOC
        
            cone_length = 1 + len(node.arglist)
            #print cone_length
            
            while cone_length > self.cone_size:
                # maximum number of elements on the lhs
                max_lhs = self.cone_size - 1
            
                # collect the new arguments
                new_args = []
                old_args = []
                count = 0
                for e in node.arglist:
                    if count < max_lhs: new_args.append(e)
                    else: old_args.append(e)
                    count += 1
                
                new_var = self.__create_variable(node.shape)                        

                # now add to varlength, varstart, and num_vars
                self.varlength[new_var.value] = ast.Dimension(node.shape.row)
                self.varstart[new_var.value] = self.num_vars
                self.num_vars += ast.Dimension(node.shape.row)
                
                # process the new cone, which has the right size
                super(MatlabCodegen,self).visit_SOCProd(ast.SOCProd(new_var, new_args))
                
                # process the old cone
                old_args.append(new_var)
            
                node.arglist = old_args
                cone_length -= (max_lhs - 1) # the extra "1" is the rhs

            if cone_length < self.cone_size:
                # create a new variable and append to the node
                new_length = self.cone_size - cone_length
                for i in range(new_length):
                    new_var = self.__create_variable(node.shape)
                    node.arglist.append(new_var)
            
                    # now add to varlength, varstart, and num_vars
                    self.varlength[new_var.value] = ast.Dimension(node.shape.row)
                    self.varstart[new_var.value] = self.num_vars
                    self.num_vars += ast.Dimension(node.shape.row)
            
        super(MatlabCodegen,self).visit_SOCProd(node)
