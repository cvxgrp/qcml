from .. ast import constraints

from .. ast.expressions import Variable
from .. properties import shape


x = Variable('x', shape.Scalar())
y = Variable('y', shape.Vector(10))
z = Variable('z', shape.Scalar())

# check that the constructors work as expected
cone_test = [
    (constraints.LinearInequality(x,y), 'x + -1*y <= 0'),
    (constraints.LinearEquality(x,z), 'x + -1*z == 0'),
    (constraints.SOC(x,[y, z]), 'norm([y; z]) <= x'),
    (constraints.SOCProd(y,[y, y]), 'norm(y, y) <= y')
]

def check(r,s):
    print r
    assert(r == s)

def test_constructors():
    for r,s in cone_test:
        check(str(r),s)
