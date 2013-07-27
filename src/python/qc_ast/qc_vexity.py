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

@use('vexity')
def isconstant(x):
    return isinstance(x, Constant)

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
        if isconstant(self) and isconstant(other): return Constant()
        if isaffine(self) and isaffine(other): return Affine()
        if isconvex(self) and isconvex(other): return Convex()
        if isconcave(self) and isconcave(other): return Concave()
        return Nonconvex()

    def __sub__(self,other):
        if isconstant(self) and isconstant(other): return Constant()
        if isaffine(self) and isaffine(other): return Affine()
        if isconvex(self) and isconcave(other): return Convex()
        if isconcave(self) and isconvex(other): return Concave()
        return Nonconvex()

    # IMPORTANT: __mul__ is missing since it requires knowledge of the sign
    # of the parent expression
    #
    # see qc_ast.qc_expression's "_signed_multiply" function
    #

    def __neg__(self):
        if isconstant(self): return Constant()
        if isaffine(self): return Affine()
        if isconvex(self): return Concave()
        if isconcave(self): return Convex()
        return Nonconvex()

    def __str__(self):
        if isconstant(self): return "constant"
        if isaffine(self): return "affine"
        if isconvex(self): return "convex"
        if isconcave(self): return "concave"
        return "nonconvex"

class Nonconvex(AbstractVexity): pass

class Convex(AbstractVexity): pass

class Concave(AbstractVexity): pass

class Affine(Convex,Concave): pass  # for affine expressions

class Constant(Affine): pass    # for numeric literals and parameters
