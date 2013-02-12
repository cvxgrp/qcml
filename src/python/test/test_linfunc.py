from scoop.expression.linfunc import Coeff, LinearFunc
from scoop.codegen import Row, Col
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
    (d - a, "0"),
    (b*e, "-3.0"),
    (a*c, "a*b"),
    (h, "2*a*a + -2.0")
]

coeff_dimensions = [
    (a + b, (Row('a'), Col('a'))),
    (e + b + d + a, (Row('a'), Col('a'))),
    (-(a + b), (Row('a'), Col('a'))),
    (d - a, (Row('a'), Col('a'))),
    (b*e, (Row(1), Col(1))),
    (a*c, (Row('a'), Col('b'))),
    (h, (Row('a'), Col('a')))
]


def check(r,s):
    print r
    assert(r == s)

def test_coeff_operators():
    for r, exp in experiments:
        yield check, str(r), exp
    for r, exp in coeff_dimensions:
        yield check, (r.rows, r.cols), exp
    
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
    (g1*g2*x, "2.0*a*a*x + -2.0*x"),
    (f1*x - f1*x, "0")
]

linfunc_dimensions = [
    (x + y, {'x': (Row(1), Col(1)), 'y': (Row(1), Col(1))}),
    (f1 + f3, {'1': (Row(1), Col(1))}),
    (f1*x + f3*x - f5*y, {'x': (Row(1), Col(1)), 'y': (Row(1), Col(1))}),
    (-(x + y), {'x': (Row(1), Col(1)), 'y': (Row(1), Col(1))}),
    ((f1 + f2)*x + y, {'x': (Row('c'), Col('c')), 'y': (Row('c'), Col(1))}),
    (g1*g2*x, {'x': (Row('a'), Col('a'))}),
    (f1*x - f1*x, {'1': (Row(1), Col(1))})
]

def test_linfunc_operators():    
    for r, exp in linfunc_experiments:
        yield check, str(r), exp
    yield assert_raises, Exception, operator.mul, x, y
    for r, exp in linfunc_dimensions:
        yield check, r.get_dimensions(), exp

linfunc_const = [
    (f1.constant_value(), 2.3),
    (f3.constant_value(), -6.3),
    ((f1+f3).constant_value(), -4.0),
    (f2.constant_value(), None)
]

def test_linfunc_const_val():
    for r, exp in linfunc_const:
        yield check, r, exp
# linfunc_bools = [
#     (f1.isconstant(), True),
#     (f3.isconstant(), True),
#     ((f1-f1).iszero(), True)
# ]
# 
# 
# def test_constant():
#     for r, exp in linfunc_bools:
#         yield check, r, exp