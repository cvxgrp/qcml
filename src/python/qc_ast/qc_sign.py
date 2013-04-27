def ispositive(x):
    return isinstance(x.sign, Positive)

def isnegative(x):
    return isinstance(x.sign, Negative)

def isneither(x):
    return isinstance(x.sign, Neither)

class Sign(object):
    def __add__(self,other): return self
    def __sub__(self,other): return self
    def __mul__(self,other): return self
    def __neg__(self): return self
    def __str__(self): return "neither"

class Neither(Sign): pass

class Positive(Sign):
    def __add__(self,other):
        if isinstance(other,Positive):
            return self
        else:
            return Neither()
    
    def __sub__(self,other):
        if isinstance(other,Negative):
            return self
        else:
            return Neither()
    
    def __mul__(self,other): 
        if isinstance(other,Positive):
            return self
        elif isinstance(other,Negative):
            return other
        else:
            return Neither()
    
    def __neg__(self): return Negative()

    def __str__(self): return "positive"

class Negative(Sign):
    def __add__(self,other):
        if isinstance(other,Negative):
            return self
        else:
            return Neither()
    
    def __sub__(self,other):
        if isinstance(other,Positive):
            return self
        else:
            return Neither()
    
    def __mul__(self,other): 
        if isinstance(other,Positive):
            return self
        elif isinstance(other,Negative):
            return Positive()
        else:
            return Neither()
    
    def __neg__(self): return Positive()

    def __str__(self): return "negative"

    

