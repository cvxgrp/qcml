import qc_square_over_lin as sol
from qcml.qc_ast import Number
from utils import annotate

""" This is the inv_pos atom.

        inv_pos(x) = square_over_lin(1,x)

    It is a CONVEX atom. It is DECREASING in the first argument.

    It is always POSITIVE.

    It returns a VECTOR expression.

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
def attributes(x):
    return sol.attributes(Number(1.0), x)

@annotate('inv_pos')
def rewrite(p,x):
    """ Rewrite an inv_pos node

        p
            the parent node

        x
            the argument
    """
    return sol.rewrite(p,Number(1.0),x)


