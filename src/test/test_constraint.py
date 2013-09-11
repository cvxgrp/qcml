import qcml.constraints.linear as linear
import qcml.constraints.soc as soc

import qcml.expressions.expression as expression
import qcml.properties.shape as shape


x = expression.Variable('x', shape.Scalar())
y = expression.Variable('y', shape.Vector(10))
z = expression.Variable('z', shape.Scalar())

# check that the constructors work as expected
cone_test = [
    (linear.LinearInequality(x,y), 'x + -1*y <= 0'),
    (linear.LinearEquality(x,z), 'x + -1*z == 0'),
    (soc.SOC(x,[y, z]), 'norm([y; z]) <= x'),
    (soc.SOCProd(y,[y, y]), 'norm(y, y) <= y')
]

def check(r,s):
    print r
    assert(r == s)

def test_constructors():
    for r,s in cone_test:
        check(str(r),s)
