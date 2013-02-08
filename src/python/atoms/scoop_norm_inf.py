from scoop.expression import Constant
from scoop_max import max_
from scoop_abs import abs_
import operator

# unfortunately, this won't lookup abs to see if it's been called already
def norm_inf(*args):
    results = map(abs_, args)
    all_lines, all_results = zip(*results)
    all_comments = map(lambda x,y:"## '%s' replaces 'abs'(%s)" % (y, x.name), args, all_results)
    max_lines, v = max_(*all_results)
    
    new_lines = map(lambda x,y:[x] + y, all_comments, all_lines)
    return (reduce(operator.add,new_lines) + max_lines, v)

