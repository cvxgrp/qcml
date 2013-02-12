# atoms from separate files
from scoop_abs import abs_
from scoop_geo_mean import geo_mean
from scoop_inv_pos import inv_pos
from scoop_max import max_
from scoop_min import min_
from scoop_neg import neg
from scoop_norm_inf import norm_inf
from scoop_norm import norm
from scoop_norm1 import norm1
from scoop_pos import pos
from scoop_pow_rat import pow_rat
from scoop_qol import quad_over_lin
from scoop_sqrt import sqrt
from scoop_square import square
from scoop_sum import sum_

# build dictionary of methods (matching with identifiers)
macros = \
    {
        'abs': abs_,
        'geo_mean': geo_mean,
        'inv_pos': inv_pos,
        'max': max_,
        'min': min_,
        'neg': neg,
        'norm_inf': norm_inf,
        'norm': norm,
        'norm2': norm,
        'norm1': norm1,
        'pos': pos,
        'pow_rat': pow_rat,
        'quad_over_lin': quad_over_lin,
        'sqrt': sqrt,
        'square': square,
        'sum': sum_
    }

