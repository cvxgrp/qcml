from scoop.expression import Constant
from scoop_qol import quad_over_lin
from utils import comment

@comment
def inv_pos(x): 
    return quad_over_lin(Constant(1.0), x)   

  