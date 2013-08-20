# from scoop.expression.constraint import Cone
# from scoop.expression import Variable, Parameter, Constant, \
#     Scalar, \
#     POSITIVE, NEGATIVE, UNKNOWN
# from nose.tools import assert_raises
#
#
# x = Variable('x', Scalar())
# y = Variable('y', Scalar())
# t = Variable('t', Scalar())
#
# # test expected cones
# cone_test = [
#     (Cone.zero(x), 'x == 0'),
#     (Cone.linear(x), 'x >= 0'),
#     (Cone.SOC(x), 'x >= 0'),
#     (Cone.SOC(t,x), 'abs(x) <= t'),
#     (Cone.SOC(t,x,y), 'norm(x,y) <= t'),
#     (Cone.SOC(t,[x]), 'norm(x) <= t'),
#     (Cone.SOC(t,[x,y]), 'norm([x;y]) <= t'),
#     (Cone.SOC(t,[]), 't >= 0'),
# ]
#
#
# def check(r,s):
#     print r
#     assert(r == s)
#
# def test_constructors():
#     for r,s in cone_test:
#         check(str(r),s)
#     yield assert_raises, Exception, Cone, 'n', t