from qcml.expressions.ast import NodeVisitor
from qcml.properties.shape import isscalar
from qcml.properties.curvature import isconstant
from qcml.expressions.qc_ast import RelOp, SOC, SOCProd
from coeff_expr import *

import itertools

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

    def __init__(self, dims = None):
        """ Walks the tree and creates the data structures.
            Creates the solver.
        """
        self.prog = []          # the full program
        self.body = []          # just the stuffing components
        self.varlength = {}
        self.varstart = {}
        self.num_vars = 0
        self.expr_stack = []
        self.num_lineqs = 0
        self.num_lps = 0
        self.num_conic = 0
        self.cone_list = []
        self.comment = '#'
        self.dims = dims    # dimension map
        self.objective_offset = 0
        self.objective_multiplier = 1

    def codegen(self):
        def not_implemented():
            raise Exception("Code generator not implemented for %s" % (self.__class__.__name__))
        return not_implemented

    def prettyprint(self,lineno=False):
        """ Pretty prints the source code, possibly with line numbers
        """
        if lineno:
            print '\n'.join("%4s    %s" % (lineno, line) for lineno, line in enumerate(self.prog))
        else:
            print '\n'.join(self.prog)

    def function_prototype(self):
        """ Gives the function prototype of the generated function.
            TODO: There may be more than one function generated....
            TODO: Supply function name...
        """
        return ""

    def function_preamble(self):
        """ Code that needs to be run once before the rest of the code can
            run.
            TODO: Maybe more than one of these needed....
        """
        return ""

    def function_datastructures(self):
        """
            varlength
                length of x variable (as int)
            num_lineqs
                number of linear equality constraints (as int)
            num_lps
                number of linear inequality constraints (as int)
            num_conic
                number of SOC constraints (as int)
            cone_list
                list of *tuple* of SOC dimensions; tuple is (num, sz), with num
                being the multiplicity of the cone size, e.g. (2, 3) means *two*
                3-dimensional cones. it is a tuple of strings
        """
        return ""

    def function_solve(self):
        """ The code we need to call to solve the problem.
        """
        return ""

    def function_recover(self,keys):
        """ The code we need to recover the solution.
        """
        return ""

    def function_stuff_c(self, start, end, expr):
        """ The code needed to stuff the c vector
        """
        return ""

    def function_stuff_b(self, start, end, expr):
        """ The code needed to stuff the b vector
        """
        return ""

    def function_stuff_h(self, start, end, expr, stride = None):
        """ The code needed to stuff the h vector
        """
        return ""

    def function_stuff_G(self, row_start, row_end, col_start, col_end, expr, row_stride = None):
        """ The code needed to stuff the sparse G matrix
        """
        return ""

    def function_stuff_A(self, row_start, row_end, col_start, col_end, expr, row_stride = None):
        """ The code needed to stuff the sparse A matrix
        """
        return ""

    def function_stuff_spmat(self):
        """ Uses I, J, V to create G and A
        """
        return ""

    def visit_Program(self, node):
        if self.dims is None:
            raise ValueError("Needed to define the dimensions of the code generator.")

        # keep track of original variables
        self.orig_varnames = set(node.variables.keys())

        # create variable ordering
        # XXX: at this moment, assumes that variables are vectors (not arrays)
        # varlength contains the vector lengths as ints
        self.varlength = {k: v.shape.eval(self.dims).size() \
            for k,v in node.variables.iteritems()
        }
        self.varlength.update({k: v.shape.eval(self.dims).size() \
            for k,v in node.new_variables.iteritems()
        })

        self.varstart = []
        for k,v in self.varlength.iteritems():
            self.varstart.append( (k, self.num_vars) )
            self.num_vars += v

        # varstart contains the start indicies as ints
        self.varstart = dict(self.varstart)

        # visit all the node
        self.generic_visit(node)

        # now, write the program
        self.prog.append(self.function_prototype())
        self.prog.append(self.offset + x for x in self.function_preamble())
        self.prog.append(self.offset + "%s '%s' has shape %s" % \
            (self.comment, v, v.shape.eval(self.dims)) \
            for v in node.parameters.values())
        self.prog.append([""])
        self.prog.append(self.offset + x for x in self.function_datastructures())

        self.prog.append(self.offset + x for x in itertools.chain.from_iterable(self.body))

        self.prog.append(self.offset + x for x in self.function_stuff_spmat())
        self.prog.append(self.offset + x for x in self.function_solve())
        self.prog.append(self.offset + x for x in self.function_recover(node.variables.keys()))

        # now construct the lines
        self.prog = list(itertools.chain.from_iterable(self.prog))

    def visit_Variable(self, node):
        n = node.shape.eval(self.dims).size()
        k = node.value

        if n == 1:
            lineq = {k: ConstantCoeff(1)}
        else:
            lineq = {k: EyeCoeff(n, ConstantCoeff(1))}

        self.expr_stack.append(lineq)

    def visit_Parameter(self, node):
        node.shape.eval(self.dims)
        if isscalar(node):
            self.expr_stack.append({'1':ScalarParameterCoeff(node.value)})
        else:
            self.expr_stack.append({'1':ParameterCoeff(node.value)})

    def visit_Number(self, node):
        self.expr_stack.append({'1':ConstantCoeff(node.value)})

    def visit_Transpose(self, node):
        self.generic_visit(node)

        arg = self.expr_stack.pop()

        for k,v in arg.iteritems():
            arg[k] = arg[k].trans()

        self.expr_stack.append(arg)

    def visit_Mul(self, node):
        # at this stage, lineq stack guaranteed to contain a constant / parameter
        assert(isconstant(node.left))
        #if not isconstant(node.left):
        #    raise SyntaxError("unknown error occurred in parsing stage. multiply has non-const and non-param lefthand side.")

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
            n = node.expr.shape.eval(self.dims).size()
            arg[k] = OnesCoeff(n, ConstantCoeff(1), True) * arg[k]

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


    def visit_Objective(self, node):
        self.body.append([""])
        self.body.append(["%s stuffing the objective vector" % self.comment])

        self.generic_visit(node)

        obj = self.expr_stack.pop()
        if not node.sense == 'find':
            for k,v in obj.iteritems():
                # ignore constants
                if k == '1':
                    self.objective_offset = v.value
                    continue
                start = self.varstart[k]
                length = self.varlength[k]
                if node.sense == 'minimize':
                    objective_c = v.trans()
                    self.objective_multiplier = 1
                elif node.sense == 'maximize':
                    objective_c = (-v).trans()
                    self.objective_multiplier = -1
                self.body.append(self.function_stuff_c(start, start+length, objective_c))

        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack
        self.body.append([""])

    def visit_RelOp(self, node):
        # in canonical form, all relop's are affine <= 0 or affine == 0
        self.body.append(["%s for the constraint %s" % (self.comment, node)])

        if node.op == '==':
            start = self.num_lineqs
            self.num_lineqs += node.shape.eval(self.dims).size()
        else:
            start = self.num_lps
            self.num_lps += node.shape.eval(self.dims).size()


        self.generic_visit(node)

        right = self.expr_stack.pop()
        left = self.expr_stack.pop()

        # right should be all 0's
        assert (right['1'].value == 0), "Expected '0' on the righthand side, but got %s." % right['1']

        # copy into the appropriate block
        for k,v in left.iteritems():
            if k == '1':
                if node.op == '==':
                    self.body.append(self.function_stuff_b(start, self.num_lineqs, -v))
                else:
                    self.body.append(self.function_stuff_h(start, self.num_lps, -v))
            else:
                xstart = self.varstart[k]
                xend = xstart + self.varlength[k]
                if node.op == '==':
                    A_string = self.function_stuff_A(start, self.num_lineqs, xstart, xend, v)
                    self.body.append(A_string)
                else:
                    G_string = self.function_stuff_G(start, self.num_lps, xstart, xend, v)
                    self.body.append(G_string)

        self.body.append([""])
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack


    def visit_SOC(self, node):
        self.body.append(["%s for the SOC constraint %s" % (self.comment, node)])

        # we assume linear constraints have already been handled
        start = [self.num_lps + self.num_conic]

        start += [start[-1] + node.right.shape.eval(self.dims).size()]
        cone_length = node.right.shape.eval(self.dims).size()
        for e in node.left:
            start += [start[-1] + e.shape.eval(self.dims).size()]
            cone_length += e.shape.eval(self.dims).size()

        self.num_conic += cone_length
        self.cone_list.append( (1, str(cone_length)) )

        self.generic_visit(node)

        # copy into the appropriate block
        count = 0
        while self.expr_stack:
            e = self.expr_stack.pop()
            coneend = start.pop()
            conestart = start[-1]   # peek at top

            for k,v in e.iteritems():
                if k == '1':
                    self.body.append(self.function_stuff_h(conestart, coneend, v))
                else:
                    xstart = self.varstart[k]
                    xend = xstart + self.varlength[k]
                    self.body.append(self.function_stuff_G(conestart, coneend, xstart, xend, -v))

        self.body.append([""])
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack

    def visit_SOCProd(self, node):
        self.body.append(["%s for the SOC product constraint %s" % (self.comment, node)])

        # we assume linear constraints have already been handled
        start = self.num_lps + self.num_conic
        stride = len(node.arglist) + 1
        self.num_conic += stride * node.shape.eval(self.dims).size()

        self.cone_list.append( (node.shape.eval(self.dims).size(), str(len(node.arglist) + 1)) )

        self.generic_visit(node)

        # copy into the appropriate block
        count = stride - 1
        coneend = self.num_conic + self.num_lps

        while self.expr_stack:
            e = self.expr_stack.pop()
            conestart = start + count
            count -= 1
            for k,v in e.iteritems():
                if k == '1':
                    self.body.append(self.function_stuff_h(conestart, coneend, v, stride))
                else:
                    xstart = self.varstart[k]
                    xend = xstart + self.varlength[k]
                    self.body.append(self.function_stuff_G(conestart, coneend, xstart, xend, -v, stride))

        self.body.append([""])
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack

""" Python codegen mixin
"""
class PythonCodegen(Codegen):
    """ This poorly named function `codegen` produces Python code
    """
    def codegen(self):
        # execute bytecode to create the "solve" function
        exec '\n'.join(self.prog) in locals()
        return solve