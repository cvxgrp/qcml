from qc_sign import Positive, Negative, Neither
from qc_vexity import Convex, Concave, Affine, Nonconvex

def ispositive(x):
    return isinstance(x,Positive)

def isnegative(x):
    return isinstance(x,Negative)

def isneither(x):
    return isinstance(x,Neither)
    
def isconvex(x):
    return isinstance(x,Convex)

def isconcave(x):
    return isinstance(x,Concave)

def isaffine(x):
    return isinstance(x,Affine)

def isnonconvex(x):
    return isinstance(x,Nonconvex)
    