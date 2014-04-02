"""
Mixin for Smith form. Introduces a new variable for every * and + node.

In general, this is not a good idea. It increase the number of variables
and doesn't necessarily result in a better sparsity pattern.

It's here simply for us to try different rewritings.
"""

from .. properties.curvature import isconstant
from .. ast.expressions import Mul, Add
from variable_creation_mixin import VariableCreatorMixin

class SmithFormMixin(VariableCreatorMixin):
    """ This implements Smith form.
    """
    def __init__(self, *args, **kwargs):
        super(SmithFormMixin, self).__init__(*args, **kwargs)
        self.__in_process_of_expanding = False

    def expand_operation(self, node):
        if not isinstance(node, (Mul, Add)):
            super(SmithFormMixin, self).visit(node)
        else:
            # introduce a new variable for expr
            new_var = self.create_variable(node.shape)
            old_stack = list(self.expr_stack)
            self.expr_stack = []
            self.__in_process_of_expanding = True
            self.visit(new_var == node)
            self.__in_process_of_expanding = False
            self.expr_stack = old_stack
            self.visit_Variable(new_var)

    def visit_Mul(self, node):
        # at this stage, lineq stack guaranteed to contain a constant / parameter
        if self.__in_process_of_expanding:
            super(SmithFormMixin,self).visit_Mul(node)
        else:
            self.expand_operation(node.left)
            self.expand_operation(node.right)

            right = self.expr_stack.pop()
            left = self.expr_stack.pop()

            # left should always be known constant
            coeff = left['1']

            for k in right.keys():
                # does not optimize gamma*matrix(1, (1,n))
                right[k] =  coeff * right[k]

            self.expr_stack.append(right)

    def visit_Add(self, node):
        # introduce a new variable for expr
        if self.__in_process_of_expanding:
            super(SmithFormMixin,self).visit_Add(node)
        else:
            self.expand_operation(node.left)
            self.expand_operation(node.right)

            right = self.expr_stack.pop()
            left = self.expr_stack.pop()

            for k in right.keys():
                if left.get(k, None) is not None:
                    left[k] = left[k] + right[k]
                else:
                    left[k] = right[k]

            self.expr_stack.append(left)
