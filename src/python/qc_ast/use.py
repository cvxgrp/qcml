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