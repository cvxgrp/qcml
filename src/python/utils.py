""" Utility boolean functions. These are intended to operate on (subclasses)
    of type Node.
"""
from qc_sign import Positive, Negative, Neither
from qc_vexity import Convex, Concave, Affine, Nonconvex
#from qc_ast import Constant, Add
import qc_ast as ast

def ispositive(x):
    return isinstance(x.sign,Positive)

def isnegative(x):
    return isinstance(x.sign,Negative)

def isneither(x):
    return isinstance(x,sign,Neither)
    
def isconvex(x):
    return isinstance(x.vexity,Convex)

def isconcave(x):
    return isinstance(x.vexity,Concave)

def isaffine(x):
    return isinstance(x.vexity,Affine)

def isnonconvex(x):
    return isinstance(x.vexity,Nonconvex)


def isconstant(x):
    return isinstance(x, ast.Constant)
    
def isadd(x):
    return isinstance(x, ast.Add)

def ismul(x):
    return isinstance(x, ast.Mul)

def isparameter(x):
    return isinstance(x, ast.Parameter)
    
def isvector(x):
    return isinstance(x.shape, Vector)

def isscalar(x):
    return isinstance(x.shape, Scalar)

def ismatrix(x):
    return isinstance(x.shape, Matrix)