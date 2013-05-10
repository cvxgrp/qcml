#from scoop.qc_ast import NodeVisitor, isscalar, RelOp, SOC, SOCProd
from scoop.qc_ast import Variable, Vector
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
            Dimension.lookup = self.args    # set the dimension lookup table
            node.dimensions = []            # empty out the dimension list
        else:
            raise Exception("MatlabCodegen: Dimensions need to be defined for Matlab.")
        super(MatlabCodegen,self).visit_Program(node)
        Dimension.lookup = None    # reset the lookup table
        
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
            if num == '1':
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
        return ["result = fillintest(solver.A,solver.G,solver.dims); % don't do anything"]
    
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
            return "solver.A(%s+1:%s:%s, %s+1:%s) = %s;" % (row_start, row_stride, row_end, col_start, col_end, expr)
        else:
            return "solver.A(%s+1:%s, %s+1:%s) = %s;" % (row_start, row_end, col_start, col_end, expr)
    
    def __create_variable(self, n):
        """Creates a new, temporary variable name"""
        name = 's' + str(self.new_soc_vars)
        self.new_soc_vars += 1
        return Variable(name, Vector(n))
            
    def visit_SOC(self, node):
        if self.cone_size is not None:
            # look at the size of the SOC
        
            cone_length = 1
            for e in node.left:
                dim = Dimension(e.shape.size_str())
                # convert dimension to integer
                cone_length += int(str(dim))
            #print cone_length
            
            # if cone_length > self.cone_size:
            #     pass
                # print self.varstart
                # print self.varlength
                # create a new variable
            if cone_length < self.cone_size:
                # create a new variable and append to the node
                new_length = self.cone_size - cone_length
                new_var = self.__create_variable(new_length)
                node.left.append(new_var)
                
                # now add to varlength, varstart, and num_vars
                self.varlength[new_var.value] = Dimension(new_length)
                self.varstart[new_var.value] = self.num_vars
                self.num_vars += Dimension(new_length)
                
        super(MatlabCodegen,self).visit_SOC(node)
        
        # self.prog.append( self.offset + "%s for the SOC constraint %s" % (self.comment, node) )
        # 
        # # we assume linear constraints have already been handled
        # start = [self.num_lps + self.num_conic]
        # 
        # start += [start[-1] + Dimension(node.right.shape.size_str())]
        # self.num_conic += Dimension(node.right.shape.size_str())
        # for e in node.left:
        #     start += [start[-1] + Dimension(e.shape.size_str())]
        #     self.num_conic += Dimension(e.shape.size_str())
        # 
        # self.generic_visit(node)
        # 
        # # copy into the appropriate block
        # count = 0
        # while self.expr_stack:
        #     e = self.expr_stack.pop()
        #     conestart = start[count]
        #     coneend = start[count+1]
        #     count += 1
        #     for k,v in e.iteritems():
        #         if k == '1':
        #             self.prog.append( self.offset + \
        #                 self.function_stuff_h(conestart, coneend, v))
        #         else:
        #             xstart = self.varstart[k]
        #             xend = xstart + self.varlength[k]
        #             self.prog.append( self.offset + \
        #                 self.function_stuff_G(conestart, coneend, xstart, xend, -v))
        # 
        # self.prog.append("")
        # assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack

    # def visit_SOCProd(self, node):
    #     self.prog.append( self.offset + "%s for the SOC product constraint %s" % (self.comment, node) )
    #     
    #     # we assume linear constraints have already been handled
    #     start = self.num_lps + self.num_conic
    #     stride = len(node.arglist) + 1
    #     self.num_conic += Dimension(None, {node.shape.size_str(): stride})
    #     
    #     self.generic_visit(node)
    #     
    #     # copy into the appropriate block
    #     count = 0
    #     coneend = self.num_conic + self.num_lps
    #     
    #     while self.expr_stack:
    #         e = self.expr_stack.pop()
    #         conestart = start + Dimension(count)
    #         count += 1
    #         for k,v in e.iteritems():
    #             if k == '1':
    #                 self.prog.append( self.offset + \
    #                     self.function_stuff_h(conestart, coneend, v, stride))
    #             else:
    #                 xstart = self.varstart[k]
    #                 xend = xstart + self.varlength[k]
    #                 self.prog.append( self.offset + \
    #                     self.function_stuff_G(conestart, coneend, xstart, xend, -v, stride))
    #     
    #     self.prog.append("")
    #     assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack