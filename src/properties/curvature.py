from .. helpers import use

@use('curvature')
def isconvex(x):
    return isinstance(x, Convex)

@use('curvature')
def isconcave(x):
    return isinstance(x, Concave)

@use('curvature')
def isaffine(x):
    return isinstance(x, Affine)

@use('curvature')
def isnonconvex(x):
    return isinstance(x, Nonconvex)

@use('curvature')
def isconstant(x):
    return isinstance(x, Constant)

class AbstractCurvature(object):
    """ Curvature is an abstract base class and should never be created.
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
    # see expressions.ops's "_signed_multiply" function
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

class Nonconvex(AbstractCurvature): pass

class Convex(AbstractCurvature): pass

class Concave(AbstractCurvature): pass

class Affine(Convex,Concave): pass  # for affine expressions

class Constant(Affine): pass    # for numeric literals and parameters
