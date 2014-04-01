""" I should probably rethink these mixins.

    The idea is that if you have a NodeVisitor for problems (in particular,
    a Codegen), you can override the methods implemented in the mixins.

    I could, of course, do something like

    class A(Codegen):
        from fixed_cone_mixin import ....
"""
from . fixed_cone_mixin import FixedConeMixin
from . restricted_multiply_mixin import RestrictedMultiplyMixin
from . smith_form_mixin import SmithFormMixin
from . ssa_like_mixin import SSALikeMixin
