from collections import MutableMapping, defaultdict
import time
def profile(f):
    def wrap(*args, **kwargs):
        start = time.clock()
        result = f(*args, **kwargs)
        elapsed = time.clock() - start
        print f.__name__, "took", elapsed, "secs"
        return result
    return wrap


# getting complicated....
class Expression(MutableMapping):
    # bucket list
    def __init__(self):
        self.terms = defaultdict(list)
        super(Expression, self).__init__()

    def __mul__(self, other):
        return Mul(self, other)

    def __add__(self, other):
        return Add(self, other)

    def __iter__(self):
        for term in self.terms.iterkeys():
            yield term

    def __getitem__(self, key):
        return self.terms[key]

    def __setitem__(self, key, value):
        if isinstance(value, list):
            self.terms[key].extend(value)
        else:
            self.terms[key].append(value)

    def __delitem__(self, key):
        del self.terms[key]

    def __len__(self):
        return len(self.terms)

    def insert(self, key, value):
        self[key] = value

    @property
    def isconstant(self):
        return len(self) == 1 and self.terms['1']

    def __str__(self):
        return str(self.terms)

class Variable(Expression):
    def __init__(self, name):
        self.name = name
        # should be eyecoeff
        super(Variable, self).__init__()
        self[name] = ConstantCoeff(1)

class Parameter(Expression):
    def __init__(self, value):
        self.value = value
        super(Parameter, self).__init__()
        self['1'] = Coeff(value)

class Number(Expression):
    def __init__(self, value):
        self.value = value
        super(Number, self).__init__()
        self['1'] = ConstantCoeff(value)

class Add(Expression):
    def __init__(self, left, right):
        super(Add, self).__init__()
        self.update(left)
        self.update(right)


class Mul(Expression):
    def __init__(self, left, right):
        # ensure that left is constant
        assert(left.isconstant)
        super(Mul, self).__init__()

        self.update(left)
        self.update(right)

class Coeff(object):
    def __init__(self, *terms):
        self.terms = terms

    def __str__(self):
        return str(self.terms)

    __repr__ = __str__

class ConstantCoeff(Coeff):
    def __init__(self, value):
        self.value = value
        super(ConstantCoeff, self).__init__(value)

    def __add__(self, other):
        assert(isinstance(other, ConstantCoeff))
        return ConstantCoeff(self.value + other.value)

# some other test



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
        if not isinstance(self.name, (int,float,long)): yield self

    def factors(self):
        # multiplicative factors
        if not isinstance(self.name, (int,float,long)): yield self

    def constant_term(self):
        if isinstance(self.name, (int,float,long)):
            if self.name == 0: return identity
            else: return self.name
        else: return identity

    def constant_factor(self):
        if isinstance(self.name, (int,float,long)):
            if self.name == 1: return identity
            else: return self.name
        else: return identity

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
        return Addy(self, other)

    def __mul__(self, other):
        # distribute
        if isinstance(other,Addy):
            new_kids = []
            new_const = identity
            for c in other.children:
                result = self*c
                if result._children:
                    new_kids.append(result)
                else:
                    new_const = result._constant
            other._constant = new_const
            other._children = new_kids
            return other
        return Muly(self, other)

    __repr__ = __str__


# TODO: flatten in the constructor...
class Addy(Expr):
    def __init__(self, left, right):
        self._constant = left.constant_term() + right.constant_term()
        self._children = list(arg for l in (left.terms(), right.terms()) for arg in l)
        super(Addy, self).__init__('...')

    def __str__(self):
        return "(" + ' + '.join(map(str,self.children)) + ")"

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
            yield Expr(self._constant)
        for e in self._children:
            yield e

    __repr__ = __str__




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
            yield Expr(self._constant)
        for e in self._children:
            yield e

    __repr__ = __str__
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
#

@profile
def add_test(f):

    # g = [Expr('0')]*1000
    e = sum(f,Expr(0))#Expr('4')*Expr('2') + (Expr('1')*Expr('3') + Expr('4'))
    print e
    #print e.children

@profile
def mul_test():
    e = Expr('a')*Expr(6)*(Expr(2) + (Expr(3) * Expr('b') * Expr('c')* Expr(4)) + Expr(1))
    print e
    #print e.constant_term()
    #print e.constant_factor()
    #print e.children[0].children
    #print e.children[0].children
    #print e.children[1].children

f = [Expr(1), Expr('a'), Expr('c'), Expr(4.1)]
add_test(f)
mul_test()

