from scoop.expression import Constant
from scoop_max import max_ 

def pos(x):    
    return max_(x,Constant(0))

  
