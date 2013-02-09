from scoop.expression import Constant
from scoop_sum import sum_
from scoop_abs import abs_
from utils import comment

import operator

@comment
def norm1(*args):
    results = map(abs_, args)
    all_lines, all_results = zip(*results)
    sum_lines, v = sum_(*all_results)
    
    return (reduce(operator.add,all_lines) + sum_lines, v)
