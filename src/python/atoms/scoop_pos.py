from scoop.expression import Constant
from scoop_max import max_ 
from utils import comment

@comment
def pos(x):    
    return max_(x,Constant(0))

  
