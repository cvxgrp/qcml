from qcml.qc_ast import Constant, Parameter, Variable, Scalar

a = Constant(1)
b = Constant(2)
c = Constant(3)

def test_constant_folding():
    a = Constant(1)
    b = Constant(2)
    c = Constant(3)
    d = Variable('x', Scalar())

    assert(str(a + b + d + c) == '6 + x')

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