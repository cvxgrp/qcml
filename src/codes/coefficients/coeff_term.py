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

    Expressions have a curvature, a sign, and a shape.

    Terms have the ability to construct coefficient objects.

    Except for special operators (transpose, sum, atoms, etc.), no new objects
    are created. That is, if you add two Expressions, the result is still
    an Expression (and not a "AddExpr").

    TODO: Next, attach properties to Expressions and Terms
            Use Mixins
"""

import collections
from abc import ABCMeta

def term_updated(lhs_terms, rhs_terms, expr):
    """ This function is called once the variables have been matched.
        Thus, we only need to match the params list.

        Will replace the matching element from the bucket. Performs a linear
        search.
    """

    for i, t1 in enumerate(lhs_terms):
        for t2 in rhs_terms:
            if t1.params == t2.params:
                lhs_terms[i] = TermFactory(t1.coeff + t2.coeff, t1.params, expr)
                return True
    return False


def TermFactory(constant, params, expr):
    # TODO: use variable "type" to determine if it is a linear term or
    # nonlinear term
    if isinstance(expr, int): expr = '' # hack to allow int arguments
    if not params and not expr:
        return Constant(constant)
    else:
        if len(expr) <= 1:
            # linear term
            return LinearTerm(constant, params, expr)
        else:
            # nonlinear term
            return NonlinearTerm(constant, params, expr)

class TermBucket(object):
    """ Stores the terms as a bucket list.

        The variable name is the key, the key 1 is for parameters + constants.
    """
    def __init__(self, buckets=None):
        if buckets is None: self.term_bucket = collections.defaultdict(list)
        else: self.term_bucket = buckets

    def __setitem__(self, k, v):
        if k: key = k
        else: key = 1
        # if the new term can be used to update an old one, just return
        if term_updated(self.term_bucket[key], [v], k): return
        # otherwise, append it to the end
        self.term_bucket[key].append(v)


    def __getitem__(self, k):
        if k: return self.term_bucket[k]
        else: return self.term_bucket[1]

    def __add__(self, other):
        buckets = collections.defaultdict(list)
        for k,v in self.term_bucket.iteritems():
            buckets[k].extend(v)

        for k,v in other.term_bucket.iteritems():
            # do a linear search on the terms to see if any match
            if k in buckets and term_updated(buckets[k], v, k): continue
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
    def __init__(self, constant, linear_terms, nonlinear_terms):
        self.linear_terms = linear_terms
        self.nonlinear_terms = nonlinear_terms
        self.constant = constant

    def __add__(self, other): return Add(self, other)  # TODO: this is a func

    def __mul__(self, other): return Mul(self, other)  # TODO: this is a func

    def __str__(self):
        return ' + '.join(map(str, self.terms))

    @property
    def terms(self):
        for e in iter(self.linear_terms): yield e
        for e in iter(self.nonlinear_terms): yield e
        if self.constant != 0: yield self.constant

    def isconstant(self):
        if self.linear_terms or self.nonlinear_terms: return False
        else: return True


class _Term(Expression):
    """ Represents a multiplication.

        constant * params * expr

        where "expr" can be either a single variable or a nonlinear function
        (an atom).
    """
    def __init__(self, constant, params, expr, *args):
        self.coeff = constant
        self.params = params
        self.expr = expr
        super(_Term, self).__init__(*args)

    def __str__(self):
        return '*'.join(map(str,self.elems))

    @property
    def elems(self):
        if self.coeff == 0: yield '0'
        else:
            if self.coeff != 1: yield self.coeff
            for e in self.params: yield e
            if self.expr: yield self.expr

    @property
    def data(self):
        return self.coeff, self.params, self.expr

class LinearTerm(_Term):
    def __init__(self, constant, params, expr):
        bucket = TermBucket()
        bucket[expr] = self
        super(LinearTerm, self).__init__(constant, params, expr, 0, bucket, TermBucket())

class NonlinearTerm(_Term):
    def __init__(self, constant, params, expr):
        bucket = TermBucket()
        bucket[expr] = self
        super(NonlinearTerm, self).__init__(constant, params, expr, 0, TermBucket(), bucket)

class Constant(_Term):
    def __init__(self, value):
        super(Constant,self).__init__(value,[], '', value, TermBucket(), TermBucket())

    #def __str__(self): return str(self.constant)

class Variable(LinearTerm):
    def __init__(self, name):
        self.name = name
        super(Variable,self).__init__(1,[],self.name)

    #def __str__(self): return self.name

class Param(LinearTerm):
    def __init__(self, name):
        self.name = name
        super(Param,self).__init__(1,[self.name],'')

    #def __str__(self): return self.name

class Atom(NonlinearTerm):
    def __init__(self, name, arg):
        self.name = name
        self.arg = arg
        super(Atom, self).__init__(1, [], "%s(%s)" % (self.name, self.arg) )


# ADD and MUL should be functions
class Add(Expression):
    def __init__(self, left, right):
        super(Add, self).__init__(left.constant + right.constant,
            left.linear_terms + right.linear_terms,
            left.nonlinear_terms + right.nonlinear_terms)

def getdata(x):
    if hasattr(x, 'data'): return x.data
    else: return (x, [], '')


class Mul(Expression):
    def __init__(self, left, right):
        # performs a distributed multiplication
        result = [0, TermBucket(), TermBucket()]

        # distribute the multiply
        for l in iter(left.terms):
            for r in iter(right.terms):
                lcoeff, lparams, lvariable = getdata(l)
                rcoeff, rparams, rvariable = getdata(r)
                if lvariable and rvariable:
                    raise ValueError("Cannot multiply two variable expressions")
                if lvariable: variable = lvariable
                else: variable = rvariable
                x = TermFactory(lcoeff*rcoeff, lparams + rparams, variable)
                if isinstance(x, Constant): result[0] = x.constant
                if isinstance(x, LinearTerm): result[1][variable] = x
                if isinstance(x, NonlinearTerm): result[2][variable] = x

        super(Mul, self).__init__(*result)


print Variable('x')
print Param('b')
print Constant(5)

y1 = Constant(5) + Constant(6)
assert(str(y1) == '11')

y2 = Constant(5) + Variable('x') + Constant(6) + Param('c')
assert(str(y2) == 'x + c + 11')
y3 = y1*y2

assert(str(y3) == '11*x + 11*c + 121')
assert(str(y1) == '11')
assert(str(y2) == 'x + c + 11')

y4 = Param('b') + Param('c') + Constant(2)
assert(str(y4) == 'b + c + 2')
assert(str(y4*y2) == 'b*x + c*x + 2*x + b*c + 11*b + c*c + 13*c + 22')

x = Variable('x')
y5 = x + x
assert(str(y5) == '2*x')
assert(str(x) == 'x')

y6 = Constant(6)*Param('b') + Constant(2.3)*Variable('x') + Param('b') + Variable('x')
assert(str(y6 + y5) == '7*b + 5.3*x')

y7 = Atom("square", y1)
assert(str(y7) == 'square(11)')

y8 = y7 + y4 + y7
assert(str(y8) == 'b + c + 2*square(11) + 2')

y9 = y4*y7 + y7
assert(str(y9) == 'b*square(11) + c*square(11) + 3*square(11)')

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
