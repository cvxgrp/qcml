import expression as expr

# utility functions for handling error messages
def error_msg(s):
    def err():
        raise Exception(s)
    return err

def id_wrapper(s):
    def id_return():
        return s
    return id_return

# bitwise masks (AFFINE|CONVEX) = CONVEX, etc.
def iscvx(e):
    return (e.vexity | expr.CONVEX) is expr.CONVEX
def isccv(e):
    return (e.vexity | expr.CONCAVE) is expr.CONCAVE
def isaff(e):
    return e.vexity is expr.AFFINE

# sign checks
def ispositive(x):
    return x.sign.sign_str is 'POSITIVE'
def isnegative(x):
    return x.sign.sign_str is 'NEGATIVE'
def isunknown(x):
    return x.sign.sign_str is 'UNKNOWN'

# vexity inference using monotonicty
def increasing(op):
    return op.vexity

def decreasing(op):
    if op.vexity is expr.CONVEX: return expr.CONCAVE
    elif op.vexity is expr.CONCAVE: return expr.CONVEX
    else: return op.vexity

def nonmonotone(op):
    if op.vexity is expr.AFFINE: return expr.AFFINE
    else: return expr.NONCONVEX