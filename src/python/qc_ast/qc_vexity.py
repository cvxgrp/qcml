def isconvex(x):
    return isinstance(x.vexity, Convex)

def isconcave(x):
    return isinstance(x.vexity, Concave)

def isaffine(x):
    return isinstance(x.vexity, Affine)

def isnonconvex(x):
    return isinstance(x.vexity, Nonconvex)

# vexity inference using monotonicty
def increasing(x):
    return x.vexity

def decreasing(x):
    return -x.vexity

def nonmonotone(x):
    if isaffine(x): return x.vexity
    else: return Nonconvex()
    
class Vexity(object):
    def __add__(self,other): return self
    def __sub__(self,other): return self
    def __neg__(self): return self
    def __str__(self): return "nonconvex"

class Nonconvex(Vexity): pass

class Convex(Vexity):
    def __add__(self,other):
        if isinstance(other,Convex):
            return self
        else:
            return Nonconvex()
    
    def __sub__(self,other):
        if isinstance(other,Concave):
            return self
        else:
            return Nonconvex()
    
    def __neg__(self): return Concave()

    def __str__(self): return "convex"

class Concave(Vexity):
    def __add__(self,other):
        if isinstance(other,Concave):
            return self
        else:
            return Nonconvex()
    
    def __sub__(self,other):
        if isinstance(other,Convex):
            return self
        else:
            return Nonconvex()
    
    def __neg__(self): return Convex()
    
    def __str__(self): return "concave"
    

class Affine(Convex,Concave):
    def __add__(self,other):
        if isinstance(other,Vexity):
            return other
        else:
            return Nonconvex()
    
    def __sub__(self,other):
        if isinstance(other,Vexity):
            return -other
        else:
            return Nonconvex()
    
    def __neg__(self): return self
    
    def __str__(self): return "affine"
    