from scoop.expression import Constant
from scoop_sum import sum_
from scoop_abs import abs_
from utils import comment

import operator

@comment
def norm1(*args):
    results = map(abs_, args)
    all_vars, all_defs = zip(*results)
    v, d = sum_(*all_vars)
    
    return (v, reduce(operator.add,all_defs) + d)
