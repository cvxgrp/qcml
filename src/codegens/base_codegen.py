from .. ast import NodeVisitor
from .. ast.constraints import  SOC, SOCProd, LinearConstraint
from .. properties.shape import isscalar, isvector, ismatrix
from .. properties.curvature import isconstant
from .. codes import ConstantCoeff, ScalarParameterCoeff, ParameterCoeff, \
    EyeCoeff, OnesCoeff

from abc import ABCMeta, abstractmethod, abstractproperty
from collections import namedtuple
import os

CodegenVariable = namedtuple('CodegenVariable', ['start', 'length'])

def write_file(new_file, code):
    with open(new_file, 'w') as output:
        output.write(code)

""" Codegen template.

    Could also be a lot shorter.
"""
class Codegen(NodeVisitor):
    __metaclass__ = ABCMeta
    """ Abstract class. Generates code for solver by walking the problem tree.

        Does not depend on any known data types or programming language.

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

    def __init__(self):
        """ Walks the tree and creates the data structures.
            Creates the functions to stuff and un-stuff the matrices.
        """
        self.primal_vars = {}      # primal variables
        self.dual_conic_vars = {}       # dual variables (ECOS' z variable)
        self.dual_equality_vars = {}    # dual variables (ECOS' y variable)
        self.num_vars = 0
        self.expr_stack = []
        self.num_lineqs = 0
        self.num_lps = 0
        self.num_conic = 0
        self.cone_list = []
        self.objective_offset = 0
        self.objective_multiplier = 1
        self._code = {} # Could use ordereddict, but that's Python >= 2.7
        self._codekeyorder = None
        super(Codegen, self).__init__()

    @abstractproperty
    def prob2socp(self):
        pass

    @abstractproperty
    def socp2prob(self):
        pass

    @abstractproperty
    def extension(self):
        """ File extension
        """
        pass

    @abstractmethod
    def functions_setup(self, program_node):
        """ Run before matrix stuffing
        """
        pass

    @abstractmethod
    def functions_return(self, program_node):
        """ Run after matrix stuffing
        """
        pass

    @abstractmethod
    def stuff_c(self, start, end, expr):
        """ The code needed to stuff the c vector
        """
        yield ""

    @abstractmethod
    def stuff_b(self, start, end, expr):
        """ The code needed to stuff the b vector
        """
        yield ""

    @abstractmethod
    def stuff_h(self, start, end, expr, stride = None):
        """ The code needed to stuff the h vector
        """
        yield ""

    @abstractmethod
    def stuff_G(self, row_start, row_end, col_start, col_end, expr, row_stride = None):
        """ The code needed to stuff the sparse G matrix
        """
        yield ""

    @abstractmethod
    def stuff_A(self, row_start, row_end, col_start, col_end, expr, row_stride = None):
        """ The code needed to stuff the sparse A matrix
        """
        yield ""

    @abstractmethod
    def abstractdim_rewriter(self, ad):
        """ Translate a raw abstract dimension name like 'm' or 'n' into a
            name like 'dims.m' or dims('n')
        """
        return "%s" % ad

    def handle_constant_offset_in_objective(self, constant_expr):
        pass

    def codegen(self):
        # create the source code
        self.prob2socp.create()
        self.socp2prob.create()

    def save(self, name):
        """ Saves prob2socp and socp2prob to a folder called `name`.
        """
        # get the current path and create the new directory in it
        new_dir = "%(path)s/%(name)s" % {'path': os.getcwd(), 'name': name}

        # create the dictionary for the generated code
        if not os.path.exists(new_dir):
            os.makedirs(new_dir)

        # write out the functions in the code
        for k in self.codekeyorder:
            filename = "%s/%s%s" % (new_dir, self.code[k].name, self.extension)
            write_file(filename, self.code[k].source)

    @property
    def code(self):
        return self._code

    @property
    def codekeyorder(self):
        return self._codekeyorder or self.code.keys()

    @property
    def source(self, order=None):
        return map(lambda k: self.code[k].source, order or self.codekeyorder)

    @property
    def numbered_source(self, order=None):
        return map(lambda k: self.code[k].numbered_source, order or self.codekeyorder)

    def printshapes(self, program_node):
        # for function documentation
        return (
            "  '%s' has shape %s" % (v, v.shape)
            for v in program_node.parameters.values()
        )

    @property
    def pmn(self):
        yield (self.num_lineqs, self.num_conic + self.num_lps, self.num_vars)

    @property
    def conesl(self): yield self.num_lps

    def _make_codegen_variable(self, variable_obj):
        length = variable_obj.shape.size(abstractdim_rewriter=self.abstractdim_rewriter)
        start = self.num_vars
        self.num_vars += length
        return CodegenVariable(start, length)

    def visit_SOCP(self, node):
        """ Visit the Program node; stores the needed information for codegen.

            TODO: may be overkill, only need to store variables, parameters,
                and dims (do not need to also have access to problem)
        """
        self.program = node
        # keep track of original variables
       # self.orig_varnames = set(node.variables.keys())

        # create variable ordering
        # XXX: at this moment, assumes that variables are vectors (not arrays)
        self.primal_vars = {
            name: self._make_codegen_variable(variable_obj) \
            for name, variable_obj in node.variables.iteritems()
        }

        self.primal_vars.update({
            name: self._make_codegen_variable(variable_obj) \
            for name, variable_obj in node.new_variables.iteritems()
        })

        # set up the functions we want to write
        self.functions_setup()

        # now, visit all the nodes
        self.generic_visit(node)

        # after we visited all the nodes, we set up the return values
        self.functions_return()

    def visit_Variable(self, node):
        k = node.value
        n = self.primal_vars[k].length

        if n == 1:
            lineq = {k: ConstantCoeff(1)}
        else:
            lineq = {k: EyeCoeff(n, ConstantCoeff(1))}

        self.expr_stack.append(lineq)

    def visit_Parameter(self, node):
        if isscalar(node):
            self.expr_stack.append({'1':ScalarParameterCoeff(node.value)})
        # elif isvector(node):
        #     self.expr_stack.append({'1':VectorParameterCoeff(node.value)})
        else:
            dims = []
            for x in node.shape.dimensions:
                if not x.concrete:
                    dims.append(self.abstractdim_rewriter(str(x)))
                else:
                    dims.append(x)

            shape = (dims[0], 1) if isvector(node) else dims
            self.expr_stack.append({'1':ParameterCoeff(node.value, shape)})

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
            n = node.expr.shape.size(abstractdim_rewriter=self.abstractdim_rewriter)
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

    def visit_ProgramObjective(self, node):
        self.prob2socp.newline()
        self.prob2socp.add_comment("stuffing the objective vector")

        self.generic_visit(node)

        obj = self.expr_stack.pop()
        if not node.sense == 'find':
            for k,v in obj.iteritems():
                # ignore constants
                if k == '1':
                    self.handle_constant_offset_in_objective(v)
                    continue
                start, length = self.primal_vars[k]
                if node.sense == 'minimize':
                    objective_c = v.trans()
                    self.objective_multiplier = 1
                elif node.sense == 'maximize':
                    objective_c = (-v).trans()
                    self.objective_multiplier = -1
                self.prob2socp.add_lines(self.stuff_c(start, start+length, objective_c))

        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack
        self.prob2socp.newline()

    def visit_LinearConstraint(self, node):
        # in canonical form, all relop's are affine <= 0 or affine == 0
        self.prob2socp.add_comment("for the constraint %s" % node)
        self.generic_visit(node)

        if node.op == '==':
            start = self.num_lineqs
            length = node.shape.size(abstractdim_rewriter=self.abstractdim_rewriter)
            self.num_lineqs += length
            if node.dual_var:
                self.dual_equality_vars[node.dual_var] = CodegenVariable(start, length)
        else:
            start = self.num_lps
            length = node.shape.size(abstractdim_rewriter=self.abstractdim_rewriter)
            self.num_lps += length
            if node.dual_var:
                self.dual_conic_vars[node.dual_var] = CodegenVariable(start, length)

        # nothing is on RHS
        #right = self.expr_stack.pop()
        left = self.expr_stack.pop()

        # copy into the appropriate block
        for k,v in left.iteritems():
            if k == '1':
                if node.op == '==':
                    self.prob2socp.add_lines(self.stuff_b(start, self.num_lineqs, -v))
                else:
                    self.prob2socp.add_lines(self.stuff_h(start, self.num_lps, -v))
            else:
                xstart, xlength = self.primal_vars[k]
                xend = xstart + xlength
                if node.op == '==':
                    A_string = self.stuff_A(start, self.num_lineqs, xstart, xend, v)
                    self.prob2socp.add_lines(A_string)
                else:
                    G_string = self.stuff_G(start, self.num_lps, xstart, xend, v)
                    self.prob2socp.add_lines(G_string)

        self.prob2socp.newline()
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack

    def visit_LinearEquality(self, node):
        self.visit_LinearConstraint(node)

    def visit_LinearInequality(self, node):
        self.visit_LinearConstraint(node)

    def visit_SOC(self, node):
        assert (not node.dual_var), "Did not expect dual var '%s' to be associated with SOC constraints" % node.dual_var
        self.prob2socp.add_comment("for the SOC constraint %s" % node)

        # we assume linear constraints have already been handled
        start = [self.num_lps + self.num_conic]

        start += [start[-1] + node.right.shape.size(abstractdim_rewriter=self.abstractdim_rewriter)]
        cone_length = node.right.shape.size(abstractdim_rewriter=self.abstractdim_rewriter)
        for e in node.left:
            start += [start[-1] + e.shape.size(abstractdim_rewriter=self.abstractdim_rewriter)]
            cone_length += e.shape.size(abstractdim_rewriter=self.abstractdim_rewriter)

        self.num_conic += cone_length
        self.cone_list.append( (1, str(cone_length)) )

        self.generic_visit(node)

        # copy into the appropriate block
        while self.expr_stack:
            e = self.expr_stack.pop()
            coneend = start.pop()
            conestart = start[-1]   # peek at top

            for k,v in e.iteritems():
                if k == '1':
                    self.prob2socp.add_lines(self.stuff_h(conestart, coneend, v))
                else:
                    xstart, xlength = self.primal_vars[k]
                    xend = xstart + xlength
                    self.prob2socp.add_lines(self.stuff_G(conestart, coneend, xstart, xend, -v))

        self.prob2socp.newline()
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack

    def visit_SOCProd(self, node):
        assert (not node.dual_var), "Did not expect dual var '%s' to be associated with SOC product constraints" % node.dual_var
        self.prob2socp.add_comment("for the SOC product constraint %s" % node)

        # we assume linear constraints have already been handled
        start = self.num_lps + self.num_conic
        stride = node.nargs + 1
        self.num_conic += stride * node.shape.size(abstractdim_rewriter=self.abstractdim_rewriter)

        self.cone_list.append( (node.shape.size(abstractdim_rewriter=self.abstractdim_rewriter), str(node.nargs + 1)) )

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
                    self.prob2socp.add_lines(self.stuff_h(conestart, coneend, v, stride))
                else:
                    xstart, xlength = self.primal_vars[k]
                    xend = xstart + xlength
                    self.prob2socp.add_lines(self.stuff_G(conestart, coneend, xstart, xend, -v, stride))

        self.prob2socp.newline()
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack
