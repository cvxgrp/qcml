from qcml.qc_ast import NodeVisitor, isscalar, RelOp, SOC, SOCProd, Dimension
from codegen_util import *

""" Codegen template.
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
        self.prog = []          # the full program
        self.body = []          # just the stuffing components
        self.varlength = {}
        self.varstart = {}
        self.num_vars = Dimension(0)
        self.expr_stack = []
        self.num_lineqs = Dimension(0)
        self.num_lps = Dimension(0)
        self.num_conic = Dimension(0)
        self.cone_list = []
        self.comment = '#'
        self.dimension_map = None

    def prettyprint(self,lineno=False):
        """ Pretty prints the source code, possibly with line numbers
        """
        if lineno:
            print '\n'.join( map(lambda x: "%4s    %s" % (x[0],x[1]), zip( range(1,len(self.prog)+1), self.prog ))  )
        else:
            print '\n'.join(self.prog)

    def function_prototype(self, dims, params):
        """ Gives the function prototype of the generated function.
            TODO: There may be more than one function generated....
            TODO: Supply function name...
        """
        pass

    def function_preamble(self):
        """ Code that needs to be run once before the rest of the code can
            run.
            TODO: Maybe more than one of these needed....
        """
        pass

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

    def function_solve(self):
        """ The code we need to call to solve the problem.
        """
        pass

    def function_recover(self,keys):
        """ The code we need to recover the solution.
        """
        pass

    def function_stuff_c(self, start, end, expr):
        """ The code needed to stuff the c vector
        """
        pass

    def function_stuff_b(self, start, end, expr):
        """ The code needed to stuff the b vector
        """
        pass

    def function_stuff_h(self, start, end, expr, stride = None):
        """ The code needed to stuff the h vector
        """
        pass

    def function_stuff_G(self, row_start, row_end, col_start, col_end, expr, row_stride = None):
        """ The code needed to stuff the sparse G matrix
        """
        pass

    def function_stuff_A(self, row_start, row_end, col_start, col_end, expr, row_stride = None):
        """ The code needed to stuff the sparse A matrix
        """
        pass

    def visit_Program(self, node):
        dims = map(str, node.dimensions)    # also includes unused dimensions
        params = map(str, node.parameters)

        # keep track of original variables
        self.orig_varnames = set(node.variables.keys())

        # create variable ordering
        # "_" variables are original variables
        # XXX: at this moment, assumes that variables are vectors (not arrays)
        self.varlength = [('_' + k,v.shape.row) for k,v in node.variables.items()]
        self.varlength += [(k,v.shape.row) for k,v in node.new_variables.items()]

        self.varstart = []
        for k,v in self.varlength:
            self.varstart.append( (k, self.num_vars) )
            self.num_vars += v

        # varstart contains the start indicies as strings
        self.varstart = dict(self.varstart)
        # varlength contains the vector lengths as strings
        self.varlength = dict(self.varlength)

        # visit all the node
        self.generic_visit(node)

        # now, write the program
        self.prog = self.function_prototype(dims, params)
        self.prog += map(lambda x: self.offset + x, self.function_preamble())
        self.prog += map(lambda x: self.offset + x,[
            "%s '%s' has shape %s" % (self.comment, v, v.shape) for v in node.parameters.values()
        ])
        self.prog.append("")
        self.prog += map(lambda x: self.offset + x, self.function_datastructures())

        self.prog += self.body

        self.prog += map(lambda x: self.offset + x,
            self.function_solve() + self.function_recover(node.variables.keys()))


    def visit_Variable(self, node):
        n = node.shape.row
        if node.value in self.orig_varnames:
            k = '_' + node.value
        else:
            k = node.value

        if n == "1":
            lineq = {k: Constant(1)}
        else:
            lineq = {k: Eye(Dimension(n), Constant(1))}

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
            n = node.arg.shape.row
            arg[k] = Ones(n, Constant(1), True) * arg[k]

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
        self.body.append("")
        self.body.append(self.offset + "%s stuffing the objective vector" % self.comment)

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
                self.body.append(self.offset + \
                    self.function_stuff_c(start, start+length, objective_c))

        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack
        self.body.append("")

    def visit_RelOp(self, node):
        # in canonical form, all relop's are affine <= 0 or affine == 0
        self.body.append(self.offset + "%s for the constraint %s" % (self.comment, node))

        if node.op == '==':
            start = self.num_lineqs
            self.num_lineqs += node.shape.row
        else:
            start = self.num_lps
            self.num_lps += node.shape.row


        self.generic_visit(node)

        right = self.expr_stack.pop()
        left = self.expr_stack.pop()

        # right should be all 0's
        assert (right['1'].value == 0), "Expected '0' on the righthand side, but got %s." % right['1']

        # copy into the appropriate block
        for k,v in left.iteritems():
            if k == '1':
                if node.op == '==':
                    self.body.append(self.offset + self.function_stuff_b(start, self.num_lineqs, -v))
                else:
                    self.body.append(self.offset + self.function_stuff_h(start, self.num_lps, -v))
            else:
                xstart = self.varstart[k]
                xend = xstart + self.varlength[k]
                if node.op == '==':
                    string = self.function_stuff_A(start, self.num_lineqs, xstart, xend, v)
                    self.body.append(self.offset + string)
                else:
                    string = self.function_stuff_G(start, self.num_lps, xstart, xend, v)
                    self.body.append(self.offset + string)

        self.body.append("")
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack


    def visit_SOC(self, node):
        self.body.append( self.offset + "%s for the SOC constraint %s" % (self.comment, node) )

        # we assume linear constraints have already been handled
        start = [self.num_lps + self.num_conic]

        start += [start[-1] + node.right.shape.row]
        cone_length = node.right.shape.row
        for e in node.left:
            start += [start[-1] + e.shape.row]
            cone_length += e.shape.row

        self.num_conic += cone_length
        self.cone_list.append( (Dimension(1), str(cone_length)) )

        self.generic_visit(node)

        # print node.left
        # print self.expr_stack
        # copy into the appropriate block
        count = 0
        while self.expr_stack:
            e = self.expr_stack.pop()
            coneend = start.pop()
            conestart = start[-1]   # peek at top

            for k,v in e.iteritems():
                if k == '1':
                    self.body.append( self.offset + \
                        self.function_stuff_h(conestart, coneend, v))
                else:
                    xstart = self.varstart[k]
                    xend = xstart + self.varlength[k]
                    self.body.append( self.offset + \
                        self.function_stuff_G(conestart, coneend, xstart, xend, -v))

        self.body.append("")
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack

    def visit_SOCProd(self, node):
        self.body.append( self.offset + "%s for the SOC product constraint %s" % (self.comment, node) )

        # we assume linear constraints have already been handled
        start = self.num_lps + self.num_conic
        stride = len(node.arglist) + 1
        self.num_conic += Dimension(None, {node.shape.size_str(): stride})

        self.cone_list.append( (node.shape.row, str(len(node.arglist) + 1)) )

        self.generic_visit(node)

        # copy into the appropriate block
        count = stride - 1
        coneend = self.num_conic + self.num_lps

        while self.expr_stack:
            e = self.expr_stack.pop()
            conestart = start + Dimension(count)
            count -= 1
            for k,v in e.iteritems():
                if k == '1':
                    self.body.append( self.offset + \
                        self.function_stuff_h(conestart, coneend, v, stride))
                else:
                    xstart = self.varstart[k]
                    xend = xstart + self.varlength[k]
                    self.body.append( self.offset + \
                        self.function_stuff_G(conestart, coneend, xstart, xend, -v, stride))

        self.body.append("")
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack

""" Python codegen mixin
"""
class PythonCodegen(Codegen):
    def codegen(self):
        # execute bytecode to create the "solve" function
        exec '\n'.join(self.prog) in locals()
        # TOOD?: wrap the solve function so that params dict is unwrapped
        # def wrapper(params):
        #     solve(**params)
        # return the wrapped function
        return solve