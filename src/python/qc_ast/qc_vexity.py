from use import use

@use('vexity')
def isconvex(x):
    return isinstance(x, Convex)

@use('vexity')
def isconcave(x):
    return isinstance(x, Concave)

@use('vexity')
def isaffine(x):
    return isinstance(x, Affine)

@use('vexity')
def isnonconvex(x):
    return isinstance(x, Nonconvex)

# vexity inference using monotonicty
def increasing(x):
    return x.vexity

def decreasing(x):
    return -x.vexity

def nonmonotone(x):
    if isaffine(x): return x.vexity
    else: return Nonconvex()


class AbstractVexity(object):
    """ Vexity is an abstract base class and should never be created.
    """
    def __add__(self,other):
        if isaffine(self) and isaffine(other): return Affine()
        if isconvex(self) and isconvex(other): return Convex()
        if isconcave(self) and isconcave(other): return Concave()
        return Nonconvex()

    def __sub__(self,other):
        if isaffine(self) and isaffine(other): return Affine()
        if isconvex(self) and isconcave(other): return Convex()
        if isconcave(self) and isconvex(other): return Concave()
        return Nonconvex()

    def __neg__(self):
        if isaffine(self): return Affine()
        if isconvex(self): return Concave()
        if isconcave(self): return Convex()
        return Nonconvex()

    def __str__(self):
        if isaffine(self): return "affine"
        if isconvex(self): return "convex"
        if isconcave(self): return "concave"
        return "nonconvex"

class Nonconvex(AbstractVexity): pass

class Convex(AbstractVexity): pass

class Concave(AbstractVexity): pass

class Affine(Convex,Concave): pass