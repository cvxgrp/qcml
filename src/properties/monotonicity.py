import curvature
import sign

# curvature inference using monotonicty
def increasing(x):
    return x.curvature

def decreasing(x):
    return -x.curvature

def nonmonotone(x):
    if curvature.isaffine(x): return x.curvature
    else: return curvature.Nonconvex()

def signed(x):
    # increasing if positive, decreasing if negative, nonmonotone otherwise
    if sign.ispositive(x): return increasing
    if sign.isnegative(x): return decreasing
    return nonmonotone
