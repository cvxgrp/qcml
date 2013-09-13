import atom
from utils import *

from qc_max import QC_max
from qc_abs import QC_abs

""" This is the norm1 atom.

        norm1(x) = sum(abs(x))

    It is a CONVEX atom. It is NONMONOTONE in the first argument.

    If the first argument is POSITIVE, it is INCREASING in the first argument.
    If the first argument is NEGATIVE, it is DECRASING in the first argument.

    It returns a SCALAR expressions.

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
def QC_norm1(*args):
    if len(args) == 1: return Sum(QC_abs(args[0]))
    else:
        return sum(map(QC_abs, args), Number(0))

atom.atoms['norm1'] = QC_norm1


# def attributes(*args):
#     result = map(qc_base.abs_, args)
#     if len(result) == 1:
#         return (result[0][0], result[0][1], Scalar())
#     else:
#         return reduce(lambda x, y: (x[0] + y[0], x[1] + y[1], x[2] + y[2]), result)
#
# @annotate('norm1')
# def rewrite(p,*args):
#     """ Rewrite a square node
#
#         p
#             the parent node
#
#         x
#             the argument
#     """
#     results = map(lambda x: qc_base.abs_rewrite(p,x), args)
#
#     if len(results) > 1:
#         return reduce(lambda x,y: (x[0] + y[0], x[1] + y[1]), results)
#     else:
#         return (Sum(results[0][0]), results[0][1])
