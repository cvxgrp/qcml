import atom
from utils import *

from qc_max import QC_max
from qc_abs import QC_abs

""" This is the norm_inf atom.

        norm_inf(x) = max(abs(x))

    It is a CONVEX atom. It is NONMONOTONE in the first argument.

    If the first argument is POSITIVE, it is INCREASING in the first argument.
    If the first argument is NEGATIVE, it is DECRASING in the first argument.

    It returns a SCALAR expressions.

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
def QC_norm_inf(x):
    return QC_max(QC_abs(x))

atom.atoms['norm_inf'] = QC_norm_inf
# def attributes(*args):
#     result = map(qc_base.abs_, args)
#     if len(result) == 1:
#         return (result[0][0], result[0][1], Scalar())
#     else:
#         return reduce(lambda x, y: (x[0] + y[0], x[1] + y[1], x[2] + y[2]), result)
#
#
# @annotate('norm_inf')
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
#         vs, constraints = zip(*results)
#         v, constraint = qc_max.rewrite(p, *vs)
#         return (v, reduce(operator.add, constraints) + constraint)
#     else:
#         v, constraints = qc_max.rewrite(p,results[0][0])
#         return (v, results[0][1] + constraints)
