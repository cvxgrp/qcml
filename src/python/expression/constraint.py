from utils import isaff, iscvx, isccv

def check_eq(x,y):
    return isaff(x) and isaff(y)
def check_leq(x,y):
    return iscvx(x) and isccv(y)
def check_geq(x,y):
    return isccv(x) and iscvx(y)

# TODO: parse leq and geq with norm taken into account
def build_eq(x,y):
    xval, yval = None, None
    if hasattr(x, 'value'): xval = x.value
    if hasattr(y, 'value'): yval = y.value
    
    if xval is not None and yval is not None:
        if xval == yval:
            return ""
        else:
            raise Exception("Trivial infeasibility detected: '%f != %f'" % (xval, yval))
    
    if xval is not None and (xval == 0.0):
        return "%s == 0" % y.name
    if yval is not None and (yval == 0.0):
        return "%s == 0" % x.name
    
    return "%s - (%s) == 0" % (x.name, y.name)
    
def build_leq(x,y):
    xval, yval = None, None
    if hasattr(x, 'value'): xval = x.value
    if hasattr(y, 'value'): yval = y.value
    
    if xval is not None and yval is not None:
        if xval <= yval:
            return ""
        else:
            raise Exception("Trivial infeasibility detected: '%f > %f'" % (xval, yval))
    
    if xval is not None and (xval == 0.0):
        return "%s >= 0" % y.name
    if yval is not None and (yval == 0.0):
        return "-(%s) >= 0" % x.name
    return "%s - (%s) >= 0" % (y.name, x.name)
    
def build_geq(x,y):
    xval, yval = None, None
    if hasattr(x, 'value'): xval = x.value
    if hasattr(y, 'value'): yval = y.value
    
    if xval is not None and yval is not None:
        if xval >= yval:
            return ""
        else:
            raise Exception("Trivial infeasibility detected: '%f < %f'" % (xval, yval))
    
    if xval is not None and (xval == 0.0):
        return "-(%s) >= 0" % y.name
    if yval is not None and (yval == 0.0):
        return "%s >= 0" % x.name
    return "%s - (%s) >= 0" % (x.name, y.name)
    
# constraint class
class Constraint(object):
    valid_constraints = set(['EQ', 'LEQ', 'GEQ'])
    constraints = {
        'EQ': (check_eq, build_eq, '=='),
        'LEQ': (check_leq, build_leq, '<='),
        'GEQ': (check_geq, build_geq, '>=')
    }
    
    def __init__(self, lhs, rhs, kind):
        """Creates a constraint object, but also checks DCP"""
        if kind in self.valid_constraints:
            check, build, sym = self.constraints[kind]
            if check(lhs,rhs):
                self.lhs = lhs
                self.rhs = rhs
                self.kind = kind
                self.sym = sym
                self.name = build(lhs,rhs)
            else:
                raise Exception("Cannot have '%s %s %s'" % (lhs.vexity, sym, rhs.vexity))
        else:
            raise Exception("Unknown constraint type %s." % kind)
            
    # when comparing constriants to something else, that's likely the case 
    # that something like l <= x <= u has occurred
    def __le__(self,other):
        if self.kind is 'LEQ':
            return Constraint(self.rhs, other, 'LEQ')
        else:
            raise Exception("Cannot have constraints of the form 'x %s y <= z" % (self.sym))
    def __ge__(self,other):
        if self.kind is 'GEQ':
            return Constraint(self.rhs, other, 'GEQ')
        else:
            raise Exception("Cannot have constraints of the form 'x %s y >= z" % (self.sym))
            
    def __eq__(self,other):
        raise Exception("Cannot chain equality constraints.")
    
    def __repr__(self):
        return "Constraint(%s, %s, '%s')" % (self.lhs, self.rhs, self.kind)
    
    def __str__(self):
        return self.name