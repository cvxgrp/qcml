# from scoop.scoop_atoms import Evaluator
# from types import MethodType
# 
# # atoms from separate files
# from add import op_add
# from minus import op_minus
# from mult import op_mult
# from negate import op_neg

# from scoop_abs import abs_
from scoop_geo_mean import geo_mean
from scoop_inv_pos import inv_pos
from scoop_max import max_
from scoop_min import min_
from scoop_neg import neg
from scoop_norm_inf import norm_inf
# from scoop_norm import norm
from scoop_norm1 import norm1
from scoop_pos import pos
from scoop_qol import quad_over_lin
from scoop_sqrt import sqrt
from scoop_square import square
# from scoop_sum import sum_
    
# # attach the atoms to the Evaluator class
# Evaluator.add = MethodType(op_add, None, Evaluator)
# Evaluator.sub = MethodType(op_minus, None, Evaluator)
# Evaluator.mul = MethodType(op_mult, None, Evaluator)
# Evaluator.neg = MethodType(op_neg, None, Evaluator)

# build dictionary of methods (matching with identifiers)
macros = \
    {
        'geo_mean': geo_mean,
        'inv_pos': inv_pos,
        'max': max_,
        'min': min_,
        'neg': neg,
        'norm_inf': norm_inf,
        'norm1': norm1,
        'pos': pos,
        'quad_over_lin': quad_over_lin,
        'sqrt': sqrt,
        'square': square  
    }

# these are not macros
from scoop_abs import abs_
from scoop_norm import norm
from scoop_sum import sum_