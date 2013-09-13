from .. helpers import use

@use('sign')
def ispositive(x):
    return isinstance(x, Positive)

@use('sign')
def isnegative(x):
    return isinstance(x, Negative)

class AbstractSign(object):
    """ Sign is an abstract base class and should never be created.
    """

    def __add__(self,other):
        if ispositive(self) and ispositive(other): return Positive()
        if isnegative(self) and isnegative(other): return Negative()
        return Neither()

    def __sub__(self,other):
        if ispositive(self) and isnegative(other): return Positive()
        if isnegative(self) and ispositive(other): return Negative()
        return Neither()

    def __mul__(self,other):
        if ispositive(self) and ispositive(other): return Positive()
        if ispositive(self) and isnegative(other): return Negative()
        if isnegative(self) and isnegative(other): return Positive()
        if isnegative(self) and ispositive(other): return Negative()
        return Neither()

    def __neg__(self):
        if ispositive(self): return Negative()
        if isnegative(self): return Positive()
        return Neither()

    def __str__(self):
        if ispositive(self): return "positive"
        if isnegative(self): return "negative"
        return "neither"

class Neither(AbstractSign): pass

class Positive(AbstractSign): pass

class Negative(AbstractSign): pass
