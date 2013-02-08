from scoop.expression import Constant
from scoop_sum import sum_
from scoop_abs import abs_
import operator

# unfortunately, this won't lookup abs to see if it's been called already
def norm1(*args):
    results = map(abs_, args)
    all_lines, all_results = zip(*results)
    all_comments = map(lambda x,y:"## '%s' replaces 'abs'(%s)" % (y, x.name), args, all_results)
    sum_lines, v = sum_(*all_results)
    
    new_lines = map(lambda x,y:[x] + y, all_comments, all_lines)
    return (reduce(operator.add,new_lines) + sum_lines, v)
