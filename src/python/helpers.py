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


import cvxopt
import time # for benchmarking

def _convert_to_cvxopt_matrices(variables):
    try:
        import numpy as np
        import cvxopt

        cvxopt_params = {k:cvxopt.matrix(v) for k,v in variables.iteritems() if isinstance(v, np.ndarray)}
        variables.update(cvxopt_params)
    except ImportError:
        pass
    return variables

def profile(f):
    def wrap(*args, **kwargs):
        start = time.clock()
        result = f(*args, **kwargs)
        elapsed = time.clock() - start
        print f.__name__, "took", elapsed, "secs"
        return result
    return wrap

def default_locals(f):
    def wrap(self, *args, **kwargs):
        if args or kwargs:
            result = f(self, *args, **kwargs)
        else:
            # get the local calling frame
            # http://stackoverflow.com/questions/6618795/get-locals-from-calling-namespace-in-python
            import inspect
            frame = inspect.currentframe()
            try:
                variables = frame.f_back.f_locals
            finally:
                del frame

            # cast to cvxopt matrices if needed
            # if there are numpy matrices, promote them to cvxopt matrices
            variables = _convert_to_cvxopt_matrices(variables)

            result = f(self, variables, variables)
        return result
    return wrap
