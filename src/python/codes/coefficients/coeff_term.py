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
        # multiplicative factors (only parameters)
        yield self

    def variable(self):
        # variable for a specific term
        return
        yield

    def constant_term(self):
        return identity

    def constant_factor(self):
        return identity
    
    def varstr(self):
        """ The string of variables
        """
        return '*'.join(map(str, self.factors())) + '*' + '*'.join(map(str, self.variable()))

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
        if self.varstr() == other.varstr():
            c = 0
            if other.constant_term() is identity: c += 1
            else: c += other.constant_term()
            if self.constant_term() is identity: c += 1
            else: c += other.constant_term()
            return Constant(c)*self
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

class Variable(Expr):
    def __init__(self, value):
        super(Variable, self).__init__(value)
    
    # abstractmethod
    def terms(self):
        # additive terms
        yield self

    def factors(self):
        # multiplicative factors
        yield identity
    
    def variable(self):
        yield self

    def constant_term(self):
        return identity

    def constant_factor(self):
        return identity

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

###### STARTING OVER #######

""" Fundamentally, an "expression" is just a list of terms with each term
    represented by three things: a constant, a multiplication stack of
    parameters, and a variable.
    
    Variables, Parameters, and Constants are also expressions.
"""

class Expression(object):
    def __init__(self, constant, list_of_terms):
        # self.terms = {term.variable:term for term in list_of_terms}
        #   # make it a dict of terms? keyed by vars?
        self.terms = list_of_terms
        self.constant = constant
    
    def __add__(self, other): return _Add(self, other)  # TODO: this is a func
    
    def __mul__(self, other): return _Mul(self, other)  # TODO: this is a func
    
    def __str__(self):
        if self.constant == 0:
            const = []
        else:
            const = [self.constant]
        return ' + '.join(map(str, self.terms + const))
    
    def isconstant(self):
        if self.terms: return False
        else: return True

class Term(Expression):
    def __init__(self, constant, list_of_params, variable):
        self.const_coeff = constant
        self.params = list_of_params
        self.variable = variable
        super(Term,self).__init__(0,[self])
    
    def __str__(self):
        if self.const_coeff == 0: return '0'
        elems = [self.const_coeff] + self.params
        if self.variable:
            elems.append(self.variable)
        return '*'.join(map(str,elems))

class _Variable(Term):
    def __init__(self, name):
        self.name = name
        super(_Variable,self).__init__(1,[],self.name)
    
    def __str__(self): return self.name

class _Param(Term):
    def __init__(self, name):
        self.name = name
        super(_Param,self).__init__(1,[self.name],'')
    
    def __str__(self): return self.name

class _Constant(Expression):
    def __init__(self, value):
        super(_Constant,self).__init__(value,[])
    
    def __str__(self): return str(self.constant)

class _Add(Expression):
    def __init__(self, left, right):
        # no simplification
        # TODO: search through left.terms and right.terms via variable names
        # terms ought to be a "dict" or somesuch...
        #
        # create our own "mapping"-like thing that allows list addition
        #
        # a+a+a -> 3*a
        # a*c + a*c + a*c -> 3*a*c
        # a*c + b*c -> a*c + b*c
        #
        # this needs to work for params too
        super(_Add, self).__init__(left.constant + right.constant, left.terms + right.terms)

class _Mul(Expression):
    def __init__(self, left, right):
        # performs a distributed multiplication
        newlist = []
        
        # multiply out the constants
        for l in left.terms:
            coeff, params, variable = l.const_coeff, l.params, l.variable
            newlist.append(Term(coeff*right.constant, params, variable))
        for r in right.terms:
            coeff, params, variable = r.const_coeff, r.params, r.variable
            newlist.append(Term(coeff*left.constant, params, variable))
            
        # now multiply out the rest
        for l in left.terms:
            for r in right.terms:
                lcoeff, lparams, lvariable = l.const_coeff, l.params, l.variable
                rcoeff, rparams, rvariable = r.const_coeff, r.params, r.variable
                if lvariable and rvariable:
                    raise ValueError("Cannot multiply two variable expressions")
                if lvariable:
                    variable = lvariable
                else:
                    variable = rvariable
                newlist.append(Term(lcoeff*rcoeff, lparams + rparams, variable))

        super(_Mul, self).__init__(left.constant * right.constant, newlist)
        

print _Variable('x')
print _Param('b')
print _Constant(5)

y1 = _Constant(5) + _Constant(6)
print y1

y2 = _Constant(5) + _Variable('x') + _Constant(6) + _Param('c')
print y2

print y1*y2

y3 = _Param('b') + _Param('c') + _Constant(2)
print y3*y2

y4 = _Variable('x') + _Variable('x')
print y4

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

# @profile
# def add_test(f):
# 
#     # g = [Expr('0')]*1000
#     e = sum(f,Constant(0))#Expr('4')*Expr('2') + (Expr('1')*Expr('3') + Expr('4'))
#     print e
#     #print e.children
# 
# @profile
# def mul_test():
#     e = (Expr('a') + Constant(6))*(Constant(2) + (Constant(3) * Expr('b') * Expr('c')* Constant(4)) + Constant(1))
#     print e
#     #print e.constant_term()
#     #print e.constant_factor()
#     #print e.children[0].children
#     #print e.children[0].children
#     #print e.children[1].children
# 
# f = [Constant(1), Expr('a'), Constant(2)*Expr('c'), Constant(4.1), Expr('c')]
# add_test(f)
# mul_test()
# 
