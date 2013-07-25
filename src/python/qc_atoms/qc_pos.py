from qcml.qc_ast import Number, ispositive
import qc_max
from utils import annotate

""" This is the pos atom.

        pos(x) = max(x,0)

    It is a CONVEX atom. It is INCREASING in the first argument.

    It returns a VECTOR expression.

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
def attributes(x):
    return qc_max.attributes(x, Number(0.0))

@annotate('pos')
def rewrite(p,x):
    """ Rewrite a square node

        p
            the parent node

        x
            the argument
    """
    if ispositive(x):
        return (x, [])
    else:
        return qc_max.rewrite(p,x,Number(0.0))

