import expression as expr
import shape

# utility functions for handling error messages
def error_msg(T, s):
    def err():
        raise T(s)
    return err

def id_wrapper(s):
    def id_return():
        return s
    return id_return

# bitwise masks (AFFINE|CONVEX) = CONVEX, etc.
def iscvx(e):
    return (e.vexity | expr.CONVEX) == expr.CONVEX
def isccv(e):
    return (e.vexity | expr.CONCAVE) == expr.CONCAVE
def isaff(e):
    return e.vexity == expr.AFFINE

# sign checks
def ispositive(x):
    return x.sign.sign_str == 'POSITIVE'
def isnegative(x):
    return x.sign.sign_str == 'NEGATIVE'
def isunknown(x):
    return x.sign.sign_str == 'UNKNOWN'

# vexity inference using monotonicty
def increasing(op):
    return op.vexity

def decreasing(op):
    if op.vexity == expr.CONVEX: return expr.CONCAVE
    elif op.vexity == expr.CONCAVE: return expr.CONVEX
    else: return op.vexity

def nonmonotone(op):
    if op.vexity == expr.AFFINE: return expr.AFFINE
    else: return expr.NONCONVEX
    
def isconstant(e):
    return e.linfunc.isconstant()
    
def to_scoop(x):
    if hasattr(x,'scoop'): return x.scoop()
    else: return str(x)

def isscalar(x):
    return isinstance(x,shape.Scalar)
def isvector(x):
    return isinstance(x,shape.Vector)
def ismatrix(x):
    return isinstance(x,shape.Matrix)
