import qc_square_over_lin as sol
from qcml.qc_ast import increasing, decreasing, nonmonotone, \
     Positive, Negative, ispositive, isnegative, \
     Convex, Concave, Affine, Constant, Atom
from utils import annotate

""" This is the square atom.

        square(x) = quad_over_lin(x,1)

    If y is a vector, it computes the division elementwise.

    It is a CONVEX atom. It is NONMONOTONE in the first argument.

    If the first argument is POSITIVE, it is INCREASING in the first argument.
    If the first argument is NEGATIVE, it is DECRASING in the first argument.

    It returns a VECTOR expression.

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
def attributes(x):
    return sol.attributes(x, Constant(1.0))

@annotate('square')
def rewrite(p,x):
    """ Rewrite a square node

        p
            the parent node

        x
            the argument
    """
    return sol.rewrite(p,x,Constant(1.0))


