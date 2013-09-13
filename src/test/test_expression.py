from qcml.ast.expressions import Number, Parameter, Variable, Transpose, Sum
from qcml.properties.shape import Scalar, Matrix
from qcml.properties.sign import Positive, Neither

a = Number(1)
b = Number(2)
c = Number(3)
x = Variable('x', Scalar())
y = Variable('y', Scalar())
w = Parameter('w', Scalar(), Positive())
u = Parameter('u', Scalar(), Positive())
D = Parameter('D', Matrix('m', 'p'), Neither())
E = Parameter('E', Matrix('p', 'n'), Neither())

expressions = [
    (a + b + x + c, '6 + x'),
    (a + x + b + w + c, '6 + x + w'),
    (a*(x + b) + c, '5 + x'),
    (a - x, '1 + -1*x'),
    (b - a*x + c, '5 + -1*x'),
    ((x-b) + (x + a), '-1 + 2*x'),
    (a + x + x + b + c, '6 + 2*x'),
    (b*(x + c), '6 + 2*x'),
    (-(a - x), '-1 + x'),
    ((a + w)*(b + x), '2 + x + w*x + 2*w'),
    (w + x - (w + x), '0'),
    (w + x - w - x, '0'),
    (w - w + x - x, '0'),
    (b*w + b*w, '4*w'),
    (w + x + b + w, '2 + x + 2*w'),
    (w + y + b + x + c + w + x + a + y, '6 + 2*x + 2*y + 2*w'),
    (w + x + b + y + b*x + c*y, '2 + 3*x + 4*y + w'),
    (w*x + w*x + w*x, '3*w*x'),
    (x + y + w + a, '1 + x + y + w'),
    (y + w + x + a, '1 + y + w + x'),
    (x + y + w + a - (y + w + x + a), '0'),
    (a*w*b*c*x, '6*w*x'),
    (Transpose(a + w), "1 + w"),
    (Transpose(b*w*u), "2*w*u"),
    (Transpose(b*D*E)*x, "2*E'*D'*x"),
    (Sum(a + w), "1 + 1'*w"),
    (Sum(w*x + Transpose(D)*x), "1'*w*x + 1'*D'*x")
]

def constant_fold(e, expected):
    print str(e)
    assert(str(e) == expected)

def test_constant_folding():
    for expr, expected in expressions:
        yield constant_fold, expr.simplify(), expected


# from scoop.expression import Expression, Constant, Parameter, Variable, \
#     CONVEX, AFFINE, CONCAVE, Sign
#
# # def test_parameter():
# #     p = Parameter
# #     assert False
# #
# # def test_variable():
# #     assert False
# #
# # def test_constant():
# #     assert False
# #
# #
# #
# # def test_sign():
# #     assert False
# #
# # def test_addition():
# #     assert False
# #
# # def test_subtraction():
# #     assert False
# #
# # def test_multiplication():
# #     assert False
# #
# # def test_linear_op():
# #     assert False
# #
# # def vex_plus_vex(v1,v2,exp):
# #     p1 = Parameter()
# #     p2 = Parameter()
# #
# # def test_vexity():
