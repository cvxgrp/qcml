from qcml.qc_ast import Number, Parameter, Variable, Scalar

a = Number(1)
b = Number(2)
c = Number(3)
x = Variable('x', Scalar())

expressions = [
    (a + b + x + c, '6 + x'),
    (a*(x + b) + c, '5 + x'),
    (a - x, '1 + -x'),
    (b - a*x + c, '5 + -x'),
    ((x-b) + (x + a), '-1 + 2*x'),
    (a + x + x + b + c, '6 + 2*x')
]

def constant_fold(e, expected):
    print str(e)
    assert(str(e) == expected)

def test_constant_folding():
    for expr, expected in expressions:
        yield constant_fold, expr, expected


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