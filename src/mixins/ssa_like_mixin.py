"""
Mixin for SSA-like form. Introduces a new variable whenever couplings
across constraints are detected. It's named "SSA-like" after "static
single assignment" in which the result of every computation is given
a new variable name. In our context, it just means every variable
appears in a constraint once (excluding the equality constraints added
to preseve equivalence).

This also introduces new variables, so the question is whether we can
obtain a better sparsity pattern. At the moment, the answer is unknown.
"""

from .. codes import ConstantCoeff, EyeCoeff
from variable_creation_mixin import VariableCreatorMixin

class SSALikeMixin(VariableCreatorMixin):
    """ This implements the restricted multiplication behavior.
    """
    def __init__(self, *args, **kwargs):
        super(SSALikeMixin, self).__init__(*args, **kwargs)
        self.__variable_reference = {}
        self.__is_adding_new_var = False
        self.__is_objective = False

    def visit_ProgramObjective(self, node):
        self.__is_objective = True
        super(SSALikeMixin, self).visit_ProgramObjective(node)
        self.__is_objective = False

    def visit_Variable(self, node):
        k = node.value
        n = self.primal_variables[k].length

        if not self.__is_adding_new_var and \
           not self.__is_objective and \
           k in self.__variable_reference.keys():
            # introduce new variable
            new_var = self.create_variable(node.shape)
            newval = new_var.value
            # add an equality constraint
            self.__is_adding_new_var = True
            old_expr_stack = list(self.expr_stack)
            self.expr_stack = []
            self.visit(new_var == self.__variable_reference[k])
            self.__is_adding_new_var = False
            self.expr_stack = old_expr_stack

            self.__variable_reference[k] = new_var
        else:
            if not self.__is_objective:
                self.__variable_reference[k] = node
            newval = k

        if n == 1:
            lineq = {newval: ConstantCoeff(1)}
        else:
            lineq = {newval: EyeCoeff(n, ConstantCoeff(1))}

        self.expr_stack.append(lineq)
