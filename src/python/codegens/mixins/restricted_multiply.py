"""
Mixin for restricted multiplication
"""
#import qcml.codegens.codegen as codegen
from .. import codegen

import qcml.expressions.expression as expression
import qcml.properties.shape as shape
from qcml.properties.curvature import isconstant

class RestrictedMultiply(codegen.Codegen):
    """ This implements the restricted multiplication behavior.
    """
    def __init__(self, *args, **kwargs):
        super(RestrictedMultiply, self).__init__(*args, **kwargs)
        self.__new_var_counter = 0

    def create_equality_constraint_variable(self, size):
        name = "_s%d" % self.__new_var_counter
        self.__new_var_counter += 1
        v = expression.Variable(name, shape.Vector(size))

        # add it to the list of lookups for building constraints
        # doesn't matter that it's at the end, since it's only for eq
        # constraints
        self.varlength[name] = size
        self.varstart[name] = self.num_vars
        self.num_vars += size

        return v

    def expand_param(self, left, right, node):
        # ONLY FOR BINARY OPERATORS
        if left.is_matrix_param and right.is_matrix_param:
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
        """
        self.generic_visit(node)

        right = self.expr_stack.pop()
        left = self.expr_stack.pop()

        for k in right.keys():
            if left.get(k, None) is not None:
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

