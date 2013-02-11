from scoop.expression import Constant
from scoop_max import max_
from scoop_abs import abs_
from utils import comment

import operator

@comment
def norm_inf(*args):
    results = map(abs_, args)
    all_vars, all_defs = zip(*results)
    v, d = max_(*all_vars)
    
    return (v, reduce(operator.add,all_defs) + d)
    
