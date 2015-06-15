"""
Mixin for creating new variables in the code generator.
"""

from .. ast.expressions import expression
from .. properties import shape
from .. codegens.base_codegen import CodegenVariable

class VariableCreatorMixin(object):
    """ This adds a variable creation to the base code generators.
    """
    def __init__(self, *args, **kwargs):
        super(VariableCreatorMixin, self).__init__(*args, **kwargs)

    def create_variable(self, shape):
        size = shape.size(abstractdim_rewriter=self.abstractdim_rewriter)
        v = expression.Variable('', shape)

        # add it to the list of lookups for building constraints
        # doesn't matter that it's at the end, since it's only for eq
        # constraints
        self.primal_vars[v.value] = CodegenVariable(self.num_vars, size)
        self.num_vars += size

        return v
