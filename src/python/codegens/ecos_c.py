"""
Code generator for ECOS C folder.

Spits out

Makefile
socp2prob.(c/h)
prob2socp.(c/h)

Links with ECOS library.
"""
from codegen import ParameterCoeff


import qcml.expressions.expression as expression
import qcml.properties.shape as shape
from qcml.properties.curvature import isconstant
from ecos import ECOSCodegen

class ECOS_C_Codegen(ECOSCodegen):
    """ This is a bizzarre codegen.
        It first generates Python code to construct the needed data matrices.

        It then executes it to get the matrices and emit code.

        Finally, it writes out C code.
    """
    def __init__(self, params, dims = None, name = "solver", path = "/usr/local/lib"):
        self.params = params
        self.path = path    # path to ECOS
        self.name = name
        super(ECOS_C_Codegen, self).__init__(dims)

        self.new_var_counter = 0
        self.new_variables = {}

    def create_equality_constraint_variable(self, size):
        name = "_s%d" % self.new_var_counter
        self.new_var_counter += 1
        v = expression.Variable(name, shape.Vector(size))

        # add it to the list of lookups for building constraints
        # doesn't matter that it's at the end, since it's only for eq
        # constraints
        self.varlength[name] = size
        self.varstart[name] = self.num_vars
        self.num_vars += size

        return v

    def visit_Mul(self, node):
        """ For C code generation, we check for PARAMS * PARAMS and promote
            the right hand side to a new variable.
        """

        # at this stage, lineq stack guaranteed to contain a constant / parameter
        assert(isconstant(node.left))
        #if not isconstant(node.left):
        #    raise SyntaxError("unknown error occurred in parsing stage. multiply has non-const and non-param lefthand side.")

        self.generic_visit(node)

        right = self.expr_stack.pop()
        left = self.expr_stack.pop()

        # left should always be known constant
        coeff = left['1']

        # with the current simplification, Mul's are on the bottom, so right
        # only contains one variable
        expr = right.values()[0]


        if isinstance(coeff, ParameterCoeff) and \
            isinstance(expr, ParameterCoeff):
            # introduce a new variable for expr
            n = node.right.shape.eval(self.dims).size()
            new_var = self.create_equality_constraint_variable(n)

            # reset the stack and save the state
            stack = self.expr_stack
            self.expr_stack = []

            # add an equality constraint
            eq_constraint = (new_var == node.right)
            self.visit(eq_constraint)

            # restore the stack
            self.expr_stack = stack
            # now visit the variable
            self.visit_Variable(new_var)
            right = self.expr_stack.pop()

        for k in right.keys():
            # does not optimize gamma*matrix(1, (1,n))
            right[k] =  coeff * right[k]

        self.expr_stack.append(right)

    def visit_Program(self, node):
        super(ECOS_C_Codegen,self).visit_Program(node)
        exec '\n'.join(self.prog) in locals()
        solve(self.params) # execute to get matrix stuffing
        #print self.prettyprint()