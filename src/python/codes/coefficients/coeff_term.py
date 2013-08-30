""" This is just a test file for me to attempt to rewrite how coefficients 
    are currently handled.
    
    At the moment, expressions are parsed without keeping track of
    coefficients. Coefficients are then later created when traversing the
    syntax tree. So there is a whole new set of them created later.
    
    The thought is that when expressions are formed, we can already keep track
    of coefficients. This lets me merge a bunch of the functionality together
    without having to write so much code.
    
    This file is just to keep track of that progress.
"""
from itertools import izip_longest

import time
def profile(f):
    def wrap(*args, **kwargs):
        start = time.clock()
        result = f(*args, **kwargs)
        elapsed = time.clock() - start
        print f.__name__, "took", elapsed, "secs"
        return result
    return wrap



# ---- this is how it should be done ---- #
# vararg add and multiply
#
# performance *is* an issue, since we create an Add and a Mul object *every
# time*. however, the code reads nicer.
#
# now need to simplify "on the fly"

class Identity(object):
    def __mul__(self, other): return other
    def __rmul__(self,other): return other
    def __add__(self,other): return other
    def __radd__(self,other): return other

identity = Identity()


class Expr(object):
    def __init__(self, name):
        self.name = name

    # abstractmethod
    def terms(self):
        # additive terms
        yield self

    def factors(self):
        # multiplicative factors
        yield self

    def constant_term(self):
        return identity

    def constant_factor(self):
        return identity

    def __str__(self): return str(self.name)

    def __add__(self, other):
        # if isinstance(self, Addy):
        #     if isinstance(other, Addy):
        #         self.children.extend(other.children)
        #     else:
        #         self.children.append(other)
        #     return self
        # else:
        #     if isinstance(other, Addy):
        #         other.children.append(self)
        #         return other
        #     else:
        #         return Addy(self, other)
        # flatten add lists
        
        # collect constant * expr + expr into (constant + 1) * expr...
        if str(self) == str(other):
            return Constant(2)*self
        # if isinstance(self, Constant) and isinstance(other, Constant):
        #     return Constant(self.name + other.name)
        #     return Muly(Expr(2), self)
        return Addy(self, other)

    def __mul__(self, other):
        # distribute
        if isinstance(self, Addy):
            return sum((c*other for c in self.children), Constant(0))
        if isinstance(other, Addy):
            return sum((self*c for c in other.children), Constant(0))
        # if isinstance(self, Constant) and isinstance(other, Constant):
        #     return Constant(self.name * other.name)
        return Muly(self, other)

    #__repr__ = __str__

class Constant(Expr):
    def __init__(self, value):
        super(Constant, self).__init__(value)
    
    # abstractmethod
    def terms(self):
        return
        yield

    def factors(self):
        return
        yield

    def constant_term(self):
        if self.name == 0: return identity
        else: return self.name

    def constant_factor(self):
        if self.name == 1: return identity
        else: return self.name


# TODO: flatten in the constructor...
class Addy(Expr):
    def __init__(self, left, right):
        self._constant = left.constant_term() + right.constant_term()
        self._children = list(arg for l in (left.terms(), right.terms()) for arg in l)
        
        for arg in left.terms():
            print '---'
            #print arg.constant_term()
            if hasattr(arg, '_children'): print map(str,arg._children)
        for arg in right.terms():
            print '---'
            #print arg.constant_term()
            if hasattr(arg, '_children'): print map(str,arg._children)
            
        super(Addy, self).__init__('...')

    def __str__(self):
        return ' + '.join(map(str,self.children))

    def terms(self):
        # additive terms
        for c in self._children:
            yield c

    def factors(self):
        # multiplicative factors
        yield self

    def constant_term(self):
        return self._constant

    def constant_factor(self):
        return identity

    @property
    def children(self):
        if self._constant is not identity:
            yield Constant(self._constant)
        for e in self._children:
            yield e

    #__repr__ = __str__

# how do i create a Mul object that only contains the variables / parameters?
class Muly(Expr):
    def __init__(self, left, right):
        self._constant = left.constant_factor() * right.constant_factor()
        self._children = list(arg for l in (left.factors(), right.factors()) for arg in l)
        super(Muly, self).__init__('...')

    def __str__(self):
        return '*'.join(map(str,self.children))

    def terms(self):
        # additive terms
        yield self

    def factors(self):
        # multiplicative factors
        for c in self._children:
            yield c

    def constant_term(self):
        return identity

    def constant_factor(self):
        return self._constant

    @property
    def children(self):
        if self._constant is not identity:
            yield Constant(self._constant)
        for e in self._children:
            yield e

    #__repr__ = __str__
# x = Variable('x')
# y = Number(1)
# b = Parameter('b')
#
# print b + y
# print b*y
# print y + y

# test 1
# 1 + 1 + 1
# test 2
# 2 + (1*3 + 4)
# test 3
# 4*2 + (1*3 + 4)
# test 4
# a*(2 + 3*4)
# test 5
# (6 + a)*(2 + 3*b*c*4 + 1)
# test 6
# (3 + a)*x + a*x
#

@profile
def add_test(f):

    # g = [Expr('0')]*1000
    e = sum(f,Constant(0))#Expr('4')*Expr('2') + (Expr('1')*Expr('3') + Expr('4'))
    print e
    #print e.children

@profile
def mul_test():
    e = (Expr('a') + Constant(6))*(Constant(2) + (Constant(3) * Expr('b') * Expr('c')* Constant(4)) + Constant(1))
    print e
    #print e.constant_term()
    #print e.constant_factor()
    #print e.children[0].children
    #print e.children[0].children
    #print e.children[1].children

f = [Constant(1), Expr('a'), Constant(2)*Expr('c'), Constant(4.1), Expr('c')]
add_test(f)
mul_test()

