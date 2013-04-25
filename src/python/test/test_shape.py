# from scoop.expression import Scalar, Vector, Matrix, isscalar, isvector, ismatrix
# from scoop.expression.shape import Row, Col
# from nose.tools import assert_raises
# import operator
# 
# shapes = [Scalar(), Vector('x'), Matrix('a')]
# 
# def add_shape(s1,s2, exp,row,col):
#     result = s1+s2
#     assert(exp(result) and result.rows == row and result.cols == col)
#     
# def negate_shape(s):
#     v = -s
#     assert(v.rows == s.rows and v.cols == s.cols)
# 
# def mul_shape(s1,s2, exp,row,col):
#     result = s1*s2
#     assert(exp(result) and result.rows == row and result.cols == col)
# 
# def test_add():
#     add_list = [
#         (Scalar(),Scalar(), isscalar, Row(1), Col(1)),
#         (Scalar(),Vector('x'), isvector, Row('x'), Col(1)),
#         (Vector('y'),Scalar(), isvector, Row('y'), Col(1)),
#         (Vector('x'),Vector('z'), isvector, Row('x'), Col(1)),
#     ]
#     for s1,s2,exp,r,c in add_list:
#         yield add_shape, s1,s2,exp,r,c
#     fail_list = [
#         (Scalar(), Matrix('a'), TypeError),
#         (Matrix('a'), Scalar(), TypeError),
#         (Matrix('a'),Vector('x'), TypeError),
#         (Matrix('a'),Matrix('a'), TypeError), # disallowed for now
#         (Vector('x'),Matrix('a'), TypeError)
#     ]
#     for s1,s2,failure in fail_list:
#         yield assert_raises, failure, add_shape, s1,s2,None,None,None
#             
# def test_sub():
#     s1 = Scalar()
#     s2 = Scalar()
#     yield assert_raises, Exception, operator.sub, s1, s2
# 
# def test_negate():
#     for s in shapes:
#         yield negate_shape, s
# 
# def test_mul():
#     mul_list = [
#         (Scalar(),Scalar(), isscalar, Row(1), Col(1)),
#         (Scalar(),Vector('x'), isvector, Row('x'), Col(1)),
#         (Scalar(),Matrix('a'), ismatrix, Row('a'), Col('a')),
#         (Vector('x'),Scalar(), isvector, Row('x'), Col(1)),
#         (Matrix('a'),Vector('x'), isvector, Row('a'), Col(1)),
#         (Matrix('a'),Scalar(), ismatrix, Row('a'), Col('a'))
#     ]
#     for s1,s2,exp,r,c in mul_list:
#         yield mul_shape, s1,s2,exp,r,c
#     fail_list = [
#         (Vector('x'),Vector('y'), TypeError),
#         (Vector('x'),Matrix('a'), TypeError),
#         (Matrix('a'),Matrix('a'), TypeError),
#     ]
#     for s1,s2,failure in fail_list:
#         yield assert_raises, failure, mul_shape, s1,s2,None,None,None
#     
#     
#     