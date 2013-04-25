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
    