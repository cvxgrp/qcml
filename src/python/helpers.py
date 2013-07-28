import qcml
import expressions

""" Use decorator.

    @use(attr)
    def f(x):
        ...

    This decorator checks if x has the attribute "attr". If it does, it will
    call f(x.attr). Otherwise, it will call f(x).
"""

def use(attr):
    def wrap(f):
        def wrapped(x):
            if hasattr(x, attr): return f(getattr(x,attr))
            else: return f(x)
        return wrapped
    return wrap

# TODO: potentially move this code into the expressions.leaf code; do this
# accounting when creating new Variables
def _create_varname():
    """Creates a new, temporary variable name; begins with underscore."""
    name = '_t' + str(qcml.QCRewriter.varcount)
    qcml.QCRewriter.varcount += 1
    return name

def create_variable(shape):
    v = expressions.expression.Variable(_create_varname(), shape)
    qcml.QCRewriter.new_variables[v.value] = v
    return v