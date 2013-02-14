import operator
from linfunc import ZERO

# these are convenience functions to *return* the cone constructor
# 0 Cone.zero(x) means x == 0
# 1 Cone.linear(x) means x >= 0
# 2 Cone.SOC(t,x) means abs(x) <= t
# 4 Cone.SOC(t,x,y,z) means norm(x,y,z) <= t
# n Cone.SOC(t,[x]) means norm(x) <= t
# n Cone.SOC(t,[x,y,z]) means norm([x;y;z]) <= t
#
# TODO: for all of these, the first thing that happens before codegen is that 
# *all* expressions is that 
# the constant term in the linear functional is inverted (moved to the RHS)
# TODO: before codegen, also, trivially feasible constraints should be removed

def is_trivially_feasible(x, bool_op, y):
    test = (x-y).value()
    
    if test is not None:
        if bool_op(test, 0):
            return True
        else:
            xval, yval = x.value(), y.value()
            raise Exception("Trivial infeasibility detected: '%s(%f, %f)'" % (bool_op.__name__, xval, yval))
    else:
        return False
        
def move_constant_to_rhs(func):
    # problem is Gx + s = h
    # s in K
    # so s = -Gx + h
    # if s in K, then we need to return -G and h
    # so we negate the linear expression to get -G and -h,
    # then negate h (or, in our case, 'c') to get the right thing
    fneg = -func
    f = dict(fneg.linear_dict)
    c = fneg.linear_dict.get('1', None)
    if c:
        del f['1']
        return (f, -c)
    else:
        return (f, ZERO)


class Cone(object):
    
    def __init__(self,size, t, *args):
        # a cone of size 0 is the free cone
        # a cone of size 1 is the nonnegative orthant
        # a cone of size m (m is an int) is just one where len(args)==m-1
        # a cone of size 'n' is  ||x||<=t, len(args)==1, but args[0] is a list
        self.size = size
        self.t = t
        if isinstance(size, str) and not args:
            raise Exception("Cannot construct an SOC cone with no cone argument!")
        else:
            self.arglist = args
    
    def __repr__(self):
        arglist = ','.join( map(str, self.arglist) )
        return "Cone(%s,%s,%s)" % (self.size,self.t, arglist)
    
    def __str__(self):
        if self.istrivial(): return ""
        if self.size == 0:
            return "%s == 0" % str(self.t)
        elif self.size == 1:
            return "%s >= 0" % str(self.t)
        elif len(self.arglist) == 1:
            if isinstance(self.size, str):
                if len(self.arglist[0]) == 1:
                    # if singleton, norm(x) <= t
                    return "norm(%s) <= %s" % (str(self.arglist[0][0]), str(self.t))
                else:
                    # norm([*args]) <= t
                    arglist = ';'.join( map(str, self.arglist[0]) )
                    return "norm([%s]) <= %s" % (arglist, str(self.t))
            else:
                # abs(*args) <= t
                return "abs(%s) <= %s" % (str(self.arglist[0]), str(self.t))
        else:
            # norm(x,y,z,...) <= t
            arglist = ','.join( map(str, self.arglist) )
            return "norm(%s) <= %s" % (arglist, str(self.t))
    
    """Declaration of constraint in SCOOP lang"""
    scoop = __str__
    
    # def __eq__(self, other): return NotImplemented
    # def __lt__(self, other): return NotImplemented
    # def __le__(self, other): return NotImplemented
    # def __gt__(self, other): return NotImplemented
    # def __ge__(self, other): return NotImplemented
    
    __eq__ = None
    __lt__ = None
    __le__ = None
    __gt__ = None
    __ge__ = None
    
    @classmethod
    def zero(self,x):
        return Cone(0,x)
        
    @classmethod
    def linear(self,x):
        return Cone(1,x)
        
    @classmethod
    def SOC(self,t,*args):
        if not args:
            return Cone(1,t)    # linear cone
        if len(args) == 1:
            if not args[0]:
                return Cone(1,t)    # linear cone (SOC(x,[]))
            elif isinstance(args[0],list):
                # norm([*args]) <= t
                return Cone('n',t,*args)
            else:
                # abs(*args) <= t
                return Cone(2,t,*args)
        else:
            # norm(*args) <= t
            return Cone(1+len(args),t,*args)
    
    def istrivial(self):
        return False
    
    def get_all_rows(self):
        def get_linfunc(funcs):
            if len(funcs) == 1 and isinstance(funcs[0],list): return map(lambda e:e.linfunc, funcs[0])
            else: return map(lambda e:e.linfunc, funcs)
        def get_sizes(funcs):
            if len(funcs) == 1 and isinstance(funcs[0],list): return map(lambda e:e.shape, funcs[0])
            else: return map(lambda e:e.shape, funcs)
        
        linfuncs = [self.t.linfunc] + get_linfunc(self.arglist)
        rhs_and_lhs = map(move_constant_to_rhs, linfuncs)
        lhs = list( v[0] for v in rhs_and_lhs )
        rhs = list( v[1] for v in rhs_and_lhs )
        sizes = [self.t.shape] + get_sizes(self.arglist)
        # t is *always* first
        return lhs, rhs, sizes

class EqConstraint(Cone):
    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs
        super(EqConstraint, self).__init__(0, lhs-rhs)
        
    def istrivial(self):
        return is_trivially_feasible(self.lhs, operator.eq, self.rhs)

        
class LeqConstraint(Cone):
    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs
        super(LeqConstraint, self).__init__(1, rhs-lhs)
        
    def istrivial(self):
        return is_trivially_feasible(self.lhs, operator.le, self.rhs)
    
    def __repr__(self):
        return "LeqConstraint(%s, %s)" % (self.lhs, self.rhs)
    
    # def __le__(self,other):
    #     return LeqConstraint(self.rhs, other)
    # 
    # def __ge__(self,other):
    #     raise Exception("Cannot have constraints of the form 'x <= y >= z")
    # 
    # def __eq__(self,other):
    #     raise Exception("Cannot have constraints of the form 'x <= y == z")
        
        
class GeqConstraint(Cone):
    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs
        super(GeqConstraint, self).__init__(1, lhs-rhs)
    
    def istrivial(self):
        return is_trivially_feasible(self.lhs, operator.ge, self.rhs)
    
    def __repr__(self):
        return "GeqConstraint(%s, %s)" % (self.lhs, self.rhs)
    
    # def __le__(self,other):
    #     raise Exception("Cannot have constraints of the form 'x >= y <= z")
    # 
    # def __ge__(self,other):
    #     raise LeqConstraint(self.rhs, other)
    #     
    # def __eq__(self,other):
    #     raise Exception("Cannot have constraints of the form 'x >= y == z")
        
