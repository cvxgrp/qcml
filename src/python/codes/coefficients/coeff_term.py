""" This is just a test file for me to attempt to rewrite how coefficients 
    are currently handled.
    
    At the moment, expressions are parsed without keeping track of
    coefficients. Coefficients are then later created when traversing the
    syntax tree. So there is a whole new set of them created later.
    
    The thought is that when expressions are formed, we can already keep track
    of coefficients. This lets me merge a bunch of the functionality together
    without having to write so much code.
    
    This file is just to keep track of that progress.
    
    TODO: This file is now complete and ready to mingle. Armed with this, we
    need to simplify the coefficient.py module and the expressions/ modules.
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


###### STARTING OVER #######
""" This module implements constant folding for expressions. It also collects
    similar terms together, so x + x = 2*x.

    However, it avoids collecting params + constant into coefficients. For
    instance, 3*x + A*x != (diag(3) + A)*x. The reason is purely pragmatic.
    
    For C code generation, we would introduce a new variable for A*x. This
    lets us copy the parameter "A" into a distinct location instead of
    mixing its memory access with the diagonal memory access pattern.
    
    For Python code generation, we would actually execute diag(3) + A and
    copy it into the proper location.
"""
import collections

def term_updated(lhs_terms, rhs_terms):
    """ This function is called once the variables have been matched.
        Thus, we only need to match the params list.
        
        Will modify the matching element in-place in the lhs. 
    """
    for t1 in lhs_terms:
        for t2 in rhs_terms:
            if t1.params == t2.params:
                t1.const_coeff += t2.const_coeff
                return True
    return False
    

class TermBucket(object):
    """ Stores the terms as a bucket list.
        
        The variable name is the key, the key 1 is for parameters.
    """
    def __init__(self, buckets=None):
        if buckets is None: self.term_bucket = collections.defaultdict(list)
        else: self.term_bucket = buckets
    
    def __setitem__(self, k, v):
        if k: self.term_bucket[k].append(v)
        else: self.term_bucket[1].append(v)
    
    def __getitem__(self, k):
        return self.term_bucket[k]
    
    def __add__(self, other):
        buckets = collections.defaultdict(list)
        for k,v in self.term_bucket.iteritems():
            buckets[k].extend(v)
        for k,v in other.term_bucket.iteritems():
            # do a linear search on the terms to see if any match
            if k in buckets and term_updated(buckets[k], v): continue
            buckets[k].extend(v)
        return TermBucket(buckets)
    
    def __iter__(self):
        return (elem for sublist in self.term_bucket.values() for elem in sublist)
        
        

""" Fundamentally, an "expression" is just a list of terms with each term
    represented by three things: a constant, a multiplication stack of
    parameters, and a variable.
    
    Variables, Parameters, and Constants are also expressions.
"""

class Expression(object):
    def __init__(self, constant, term_bucket):
        self.terms = term_bucket
        self.constant = constant
    
    def __add__(self, other): return _Add(self, other)  # TODO: this is a func
    
    def __mul__(self, other): return _Mul(self, other)  # TODO: this is a func
    
    def __str__(self):
        return ' + '.join(map(str, self.all_terms))
    
    @property
    def all_terms(self):
        for e in iter(self.terms):
            yield e
        if self.constant != 0: yield self.constant
    
    def isconstant(self):
        if self.terms: return False
        else: return True

class Term(Expression):
    def __init__(self, constant, list_of_params, variable):
        self.const_coeff = constant
        self.params = list_of_params
        self.variable = variable
        bucket = TermBucket()
        bucket[variable] = self
        super(Term,self).__init__(0,bucket)
    
    def __str__(self):
        if self.const_coeff == 0: return '0'
        elems = [self.const_coeff] + self.params
        if self.variable:
            elems.append(self.variable)
        return '*'.join(map(str,elems))
    
    @property
    def data(self):
        return self.const_coeff, self.params, self.variable

class _Variable(Term):
    def __init__(self, name):
        self.name = name
        super(_Variable,self).__init__(1,[],self.name)
    
    #def __str__(self): return self.name

class _Param(Term):
    def __init__(self, name):
        self.name = name
        super(_Param,self).__init__(1,[self.name],'')
    
    #def __str__(self): return self.name

class _Constant(Expression):
    def __init__(self, value):
        super(_Constant,self).__init__(value,TermBucket())
    
    #def __str__(self): return str(self.constant)

class _Add(Expression):
    def __init__(self, left, right):
        super(_Add, self).__init__(left.constant + right.constant, left.terms + right.terms)

class _Mul(Expression):
    def __init__(self, left, right):
        # performs a distributed multiplication
        newlist = TermBucket()
        
        # multiply out the constants
        for l in iter(left.terms):
            coeff, params, variable = l.data
            newlist[variable] = Term(coeff*right.constant, params, variable)
        for r in iter(right.terms):
            coeff, params, variable = r.data
            newlist[variable] = Term(coeff*left.constant, params, variable)
            
        # now multiply out the rest
        for l in iter(left.terms):
            for r in iter(right.terms):
                lcoeff, lparams, lvariable = l.data
                rcoeff, rparams, rvariable = r.data
                if lvariable and rvariable:
                    raise ValueError("Cannot multiply two variable expressions")
                if lvariable:
                    variable = lvariable
                else:
                    variable = rvariable
                newlist[variable] = Term(lcoeff*rcoeff, lparams + rparams, variable)

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

y5 = _Constant(6)*_Param('b') + _Constant(2.3)*_Variable('x') + _Param('b') + _Variable('x')
print y5 + y4
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
