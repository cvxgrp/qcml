"""
Mixin for Smith form. Introduces a new variable for every * and + node.

In general, this is not a good idea. It increase the number of variables
and doesn't necessarily result in a better sparsity pattern.

It's here simply for us to try different rewritings.
"""

from .. properties.curvature import isconstant
from variable_creation_mixin import VariableCreatorMixin

class SmithFormMixin(VariableCreatorMixin):
    """ This implements the restricted multiplication behavior.
    """
    def __init__(self, *args, **kwargs):
        super(SmithFormMixin, self).__init__(*args, **kwargs)
        self.__in_process_of_expanding = False

    def expand_operation(self, node):
        # introduce a new variable for expr
        new_var = self.create_variable(node.shape)

        old_stack = list(self.expr_stack)
        self.expr_stack = []
        eq_constraint = (new_var == node)
        self.__in_process_of_expanding = True
        self.visit(eq_constraint)
        self.visit(new_var)
        self.__in_process_of_expanding = False
        var_coeff = self.expr_stack.pop()
        self.expr_stack = old_stack
        self.expr_stack.append(var_coeff)

    def visit_Mul(self, node):
        # at this stage, lineq stack guaranteed to contain a constant / parameter
        assert(isconstant(node.left))
        self.generic_visit(node)

        # discard these
        self.expr_stack.pop()
        self.expr_stack.pop()

        if self.__in_process_of_expanding:
            super(SmithFormMixin,self).visit_Mul(node)
        else:
            self.expand_operation(node)


    def visit_Add(self, node):
        self.generic_visit(node)

        # discard these
        self.expr_stack.pop()
        self.expr_stack.pop()

        # introduce a new variable for expr
        if self.__in_process_of_expanding:
            super(SmithFormMixin,self).visit_Add(node)
        else:
            self.expand_operation(node)
