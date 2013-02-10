from scoop.expression import Constant
from scoop_qol import quad_over_lin
from utils import comment

@comment
def square(x):
    return quad_over_lin(x, Constant(1.0))

