from scoop.expression import Constant
from scoop_max import max_ 

def neg(x):    
    return max_(-x,Constant(0))

  
