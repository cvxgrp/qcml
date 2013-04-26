from qc_sign import Positive, Negative, Neither
from qc_vexity import Convex, Concave, Affine, Nonconvex


class Expression(object):
    """ A "type" container for the expression tree
    """
    def __init__(self):
        self.vexity = Nonconvex()
        self.sign = Neither()
        

class Constant(Expression): pass

class Variable(Expression): pass

class Parameter(Expression): pass