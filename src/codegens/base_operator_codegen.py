from base_codegen import Codegen, CodegenVariable
from .. ast.constraints import  SOC, SOCProd, LinearConstraint
from .. properties.shape import isscalar
from .. properties.curvature import isconstant
from .. codes import ConstantCoeff, ScalarParameterCoeff, ParameterCoeff, \
    EyeCoeff, OnesCoeff

from abc import ABCMeta, abstractmethod, abstractproperty
import os

def write_file(new_file, code):
    with open(new_file, 'w') as output:
        output.write(code)

""" Operator codegen template.
"""
class OperatorCodegen(Codegen):
    __metaclass__ = ABCMeta
    """ Abstract class. Generates code for evaluating a linear operator and
        its adjoint (transpose).

        Since the cone program contains the matrix A for linear equality
        constraints and G for conic inequality constraints, this codegen
        will produce code for fA, fG, fAT, fGT, for evaluating the operator
        A, G, A^T, and G^T, respectively.

        Does not depend on any known data types or programming language.

---
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
        super(OperatorCodegen, self).__init__()

    @abstractproperty
    def fA(self):
        """ Access the fA Function object
        """
        pass

    @abstractproperty
    def fAT(self):
        """ Access the fAT Function object
        """
        pass

    @abstractproperty
    def fG(self):
        """ Access the fG Function object
        """
        pass

    @abstractproperty
    def fGT(self):
        """ Access the fGT Function object
        """
        pass

    @abstractmethod
    def stuff_G(self, row_start, row_end, col_start, col_end, expr, row_stride = None):
        """ This code stuffs a *function*. Compare to base_codegen, where this code
            is supposed to stuff a *matrix*.
        """
        yield ""

    @abstractmethod
    def stuff_A(self, row_start, row_end, col_start, col_end, expr, row_stride = None):
        """ This code stuffs a *function*. Compare to base_codegen, where this code
            is supposed to stuff a *matrix*.
        """
        yield ""

    def codegen(self):
        # create the source code
        self.fA.create()
        self.fAT.create()
        self.fG.create()
        self.fGT.create()
        super(OperatorCodegen, self).codegen()

    """
        # essentially the same as base_codegen except that i have to handle fA, fAT, etc.
    """
    def visit_LinearConstraint(self, node):
        # in canonical form, all relop's are affine <= 0 or affine == 0
        self.prob2socp.add_comment("for the constraint %s" % node)

        if node.op == '==':
            self.fA.add_comment("for the constraint %s" % node)
            self.fAT.add_comment("for the constraint %s" % node)
            start = self.num_lineqs
            length = node.shape.size(abstractdim_rewriter=self.abstractdim_rewriter)
            self.num_lineqs += length
            if node.dual_var:
                self.dual_equality_vars[node.dual_var] = CodegenVariable(start, length)
        else:
            self.fG.add_comment("for the constraint %s" % node)
            self.fGT.add_comment("for the constraint %s" % node)
            start = self.num_lps
            length = node.shape.size(abstractdim_rewriter=self.abstractdim_rewriter)
            self.num_lps += length
            if node.dual_var:
                self.dual_conic_vars[node.dual_var] = CodegenVariable(start, length)


        self.generic_visit(node)

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
                    self.stuff_A(start, self.num_lineqs, xstart, xend, v)
                else:
                    self.stuff_G(start, self.num_lps, xstart, xend, v)

        self.prob2socp.newline()
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack

    def visit_LinearEquality(self, node):
        self.visit_LinearConstraint(node)

    def visit_LinearInequality(self, node):
        self.visit_LinearConstraint(node)

    def visit_SOC(self, node):
        assert (not node.dual_var), "Did not expect dual var '%s' to be associated with SOC constraints" % node.dual_var

        self.prob2socp.add_comment("for the SOC constraint %s" % node)
        self.fG.add_comment("for the SOC constraint %s" % node)
        self.fGT.add_comment("for the SOC constraint %s" % node)

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
                    self.stuff_G(conestart, coneend, xstart, xend, -v)

        self.prob2socp.newline()
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack

    def visit_SOCProd(self, node):
        assert (not node.dual_var), "Did not expect dual var '%s' to be associated with SOC product constraints" % node.dual_var

        self.prob2socp.add_comment("for the SOC product constraint %s" % node)
        self.fG.add_comment("for the SOC product constraint %s" % node)
        self.fGT.add_comment("for the SOC product constraint %s" % node)

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
                    self.stuff_G(conestart, coneend, xstart, xend, -v, stride)

        self.prob2socp.newline()
        assert (not self.expr_stack), "Expected empty expression stack but still has %s left" % self.expr_stack
