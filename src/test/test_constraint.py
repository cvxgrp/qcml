from qcml.ast.constraints import *

from qcml.ast.expressions import Variable
import qcml.properties.shape as shape


x = Variable('x', shape.Scalar())
y = Variable('y', shape.Vector(10))
z = Variable('z', shape.Scalar())

# check that the constructors work as expected
cone_test = [
    (LinearInequality(x,y), 'x + -1*y <= 0'),
    (LinearEquality(x,z), 'x + -1*z == 0'),
    (SOC(x,[y, z]), 'norm([y; z]) <= x'),
    (SOCProd(y,[y, y]), 'norm(y, y) <= y')
]

def check(r,s):
    print r
    assert(r == s)

def test_constructors():
    for r,s in cone_test:
        check(str(r),s)
