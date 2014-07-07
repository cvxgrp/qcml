"""
Mixin for restricted multiplication
"""

from .. properties.curvature import isconstant
from .. codes.coefficients.coefficient import EyeCoeff, OnesCoeff, ConstantCoeff
from variable_creation_mixin import VariableCreatorMixin

class RestrictedMultiplyMixin(VariableCreatorMixin):
    """ This implements the restricted multiplication behavior.
    """
    def __init__(self, *args, **kwargs):
        super(RestrictedMultiplyMixin, self).__init__(*args, **kwargs)

    def expand_param(self, left, right, node):
        # ONLY FOR BINARY OPERATORS
        if (left.is_matrix_param and right.is_matrix_param):
            # introduce a new variable for expr
            new_var = self.create_variable(node.right.shape)

            # reset the stack and save the state
            stack = list(self.expr_stack)
            self.expr_stack = []

            # add an equality constraint
            eq_constraint = (new_var == node.right)
            self.visit(eq_constraint)

            # restore the stack
            self.expr_stack = stack
            # now visit the variable
            self.visit_Variable(new_var)
            return True
        else:
            return False


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


        if self.expand_param(coeff, expr, node):
            right = self.expr_stack.pop()

        for k in right.keys():
            # does not optimize gamma*matrix(1, (1,n))
            right[k] =  coeff * right[k]

        self.expr_stack.append(right)

    def visit_Add(self, node):
        """ For C code generation, we check for PARAMS + PARAMS and promote
            the right hand side to a new variable.

            TODO: Won't work if the param is a matrix, but in that case, it
            ought to have been something like PARAMS*x + PARAMS*x....
        """
        self.generic_visit(node)

        right = self.expr_stack.pop()
        left = self.expr_stack.pop()

        for k in right.keys():
            if left.get(k, None) is not None:
                # HACK to ensure that x + PARAM*x and PARAM*x + x is handled properly
                if isinstance(left[k], EyeCoeff) and not isinstance(right[k], EyeCoeff):
                    left[k].is_matrix_param = True
                if isinstance(right[k], EyeCoeff) and not isinstance(left[k], EyeCoeff):
                    right[k].is_matrix_param = True
                    
                if self.expand_param(left[k], right[k], node):
                    new_elem = self.expr_stack.pop()
                    name = new_elem.keys()[0]   # name of variable
                    left[name] = new_elem[name]
                    # left[k] = left[k], leaves left[k] intact
                else:
                    left[k] = left[k] + right[k]
            else:
                left[k] = right[k]

        self.expr_stack.append(left)

    def visit_Sum(self, node):
        self.generic_visit(node)

        arg = self.expr_stack.pop()

        n = node.expr.shape.size(abstractdim_rewriter=self.abstractdim_rewriter)
        left = OnesCoeff(n, ConstantCoeff(1), True)
        # HAX: make the sum child node.right
        node.right = list(node.children())[0]
        if self.expand_param(left, arg.values()[0], node):
            arg = self.expr_stack.pop()

        for k in arg.keys():
            arg[k] = left * arg[k]

        self.expr_stack.append(arg)

