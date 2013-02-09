from scoop.expression.linfunc import Coeff, LinearFunc
from scoop.expression import Variable, Parameter, Constant, \
    SCALAR, VECTOR, MATRIX, \
    POSITIVE, NEGATIVE, UNKNOWN
from nose.tools import assert_raises, with_setup    
import operator

a = Coeff.parameter('a')
b = Coeff.constant(1.0)
c = Coeff.parameter('b')
d = Coeff.parameter('a')
e = Coeff.constant(-3.0)
h = (a + b) * (e + b + d + a)
experiments = [
    (a + b, "a + 1.0"),
    (e + b + d + a, "2*a + -2.0"),
    (-(a + b), "-a + -1.0"),
    (d - a, ""),
    (b*e, "-3.0"),
    (a*c, "a*b"),
    (h, "2*a*a + -2.0")
]

def check(r,s):
    print str(r)
    assert(str(r) == s)

def test_coeff_operators():
    for r, exp in experiments:
        yield check, r, exp
    
f1 = LinearFunc.constant(2.3)
f2 = LinearFunc.constant('c')
f3 = LinearFunc.constant(-6.3)
f4 = LinearFunc.constant('d')
f5 = LinearFunc.constant(4.0)
g1 = LinearFunc.constant(a+b)
g2 = LinearFunc.constant(e+b+d+a)
x = LinearFunc.variable('x')
y = LinearFunc.variable('y')

linfunc_experiments = [
    (x + y, "y + x"),
    (f1 + f3, "-4.0"),
    (f1*x + f3*x - f5*y, "-4.0*y + -4.0*x"),
    (-(x + y), "-y + -x"),
    ((f1 + f2)*x + y, "y + c*x + 2.3*x"),
    (g1*g2*x, "2.0*a*a*x + -2.0*x")
]

def test_linfunc_operators():    
    for r, exp in linfunc_experiments:
        yield check, r, exp
    yield assert_raises, Exception, operator.mul, x, y
    