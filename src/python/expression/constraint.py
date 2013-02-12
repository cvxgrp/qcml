import operator

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
            return Cone(len(args),t,*args)
    
    def istrivial(self):
        return False
        
    def get_equivalence_sets(self, variables):
        """Returns a list of dicts of vector variables whose shapes are related"""
   
        if self.istrivial(): return []
        x = self.t.linfunc.get_dimensions()
        s1 = set(x)
        
        if self.size == 0:
            return [s1]
        elif self.size == 1:
            return [s1]
        elif len(self.arglist) == 1:
            if isinstance(self.size, str):
                    ys = map(lambda e:set(e.linfunc.linear_dict), self.arglist[0])
                    return [s1] + ys
            else:
                # abs(*args) <= t
                y = self.arglist[0].linfunc.linear_dict
                s2 = set(y)
                return [s1.union(s2)]
        else:
            # norm(x,y,z,...) <= t
            ys = map(lambda e:set(e.linfunc.linear_dict), self.arglist)
            s = reduce(lambda x,y: x.union(y), ys, s1)
            return [s]
    
    def get_dimension(self, s):
        linfuncs = [self.t.linfunc.linear_dict]
        if self.size !=0 and self.size != 1 and len(self.arglist) == 1:
            if isinstance(self.size, str):
                    ys = map(lambda e:e.linfunc.linear_dict, self.arglist[0])
                    linfuncs += ys
            else:
                # abs(*args) <= t
                linfuncs += [self.arglist[0].linfunc.linear_dict]
        else:
            # norm(x,y,z,...) <= t
            ys = map(lambda e:e.linfunc.linear_dict, self.arglist)
            linfuncs += ys
        
        def lookup_dim(e):
            return e.get(s, None)

        return map(lookup_dim, linfuncs)
    
    # def get_all_linfuncs(self):
    #     def get_linfunc(funcs):
    #         if len(funcs) == 1 and isinstance(funcs[0],list): return map(lambda e:e.linfunc, funcs[0])
    #         else: return map(lambda e:e.linfunc, funcs)
    # 
    #     return [self.t.linfunc] + get_linfunc(self.arglist)

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
        
