from scoop.expression import Constant
from scoop_max import max_
from scoop_abs import abs_
from utils import comment

import operator

@comment
def norm_inf(*args):
    results = map(abs_, args)
    all_lines, all_results = zip(*results)
    max_lines, v = max_(*all_results)
    
    return (reduce(operator.add,all_lines) + max_lines, v)

