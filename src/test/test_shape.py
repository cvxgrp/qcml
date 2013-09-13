from .. properties.shape import Shape, Scalar, Vector, Matrix, isvector, isscalar, ismatrix
from nose.tools import assert_raises
import operator

def add_shape(s1,s2, exp,row,col):
    result = s1+s2
    assert(exp(result) and result.row == row and result.col == col)

def sub_shape(s1,s2, exp,row,col):
    result = s1+s2
    assert(exp(result) and result.row == row and result.col == col)

def negate_shape(s):
    v = -s
    assert(v.row == s.row and v.col == s.col)

def mul_shape(s1,s2, exp,row,col):
    result = s1*s2
    assert(exp(result) and result.row == row and result.col == col)

def test_add():
    add_list = [
        (Scalar(),Scalar(), isscalar, 1, 1),
        (Scalar(),Vector(5), isvector, 5, 1),
        (Vector(5),Scalar(), isvector, 5, 1),
        (Vector(10),Vector(10), isvector, 10, 1),
        (Scalar(), Matrix(3,2), ismatrix, 3, 2),
        (Matrix(3,2), Scalar(), ismatrix, 3, 2),
        (Matrix(3,2),Matrix(3,2), ismatrix, 3,2),
    ]
    for s1,s2,exp,r,c in add_list:
        yield add_shape, s1,s2,exp,r,c
    fail_list = [
        (Vector(5), Vector(10), TypeError),
        (Matrix(3,2),Vector(4), TypeError),
        (Matrix(3,6),Matrix(7,2), TypeError), # disallowed for now
        (Vector(4),Matrix(3,2), TypeError)
    ]
    for s1,s2,failure in fail_list:
        yield assert_raises, failure, add_shape, s1,s2,None,None,None

def test_sub():
    sub_list = [
        (Scalar(),Scalar(), isscalar, 1, 1),
        (Scalar(),Vector(5), isvector, 5, 1),
        (Vector(5),Scalar(), isvector, 5, 1),
        (Vector(10),Vector(10), isvector, 10, 1),
        (Scalar(), Matrix(3,2), ismatrix, 3, 2),
        (Matrix(3,2), Scalar(), ismatrix, 3, 2),
        (Matrix(3,2),Matrix(3,2), ismatrix, 3,2),
    ]
    for s1,s2,exp,r,c in sub_list:
        yield sub_shape, s1,s2,exp,r,c
    fail_list = [
        (Vector(5), Vector(10), TypeError),
        (Matrix(3,2),Vector(4), TypeError),
        (Matrix(3,6),Matrix(7,2), TypeError), # disallowed for now
        (Vector(4),Matrix(3,2), TypeError)
    ]
    for s1,s2,failure in fail_list:
        yield assert_raises, failure, sub_shape, s1,s2,None,None,None


def test_negate():
    for s in [Scalar(), Vector(5), Matrix(10,5)]:
        yield negate_shape, s

def test_mul():
    mul_list = [
        (Scalar(),Scalar(), isscalar, 1, 1),
        (Scalar(),Vector(4), isvector, 4, 1),
        (Scalar(),Matrix(3,2), ismatrix, 3, 2),
        (Vector(4),Scalar(), isvector, 4, 1),
        (Matrix(3,4),Vector(4), isvector, 3, 1),
        (Matrix(3,2),Scalar(), ismatrix, 3,2),
        (Matrix(3,2),Matrix(2,3), ismatrix, 3, 3),
        (Vector(3), Matrix(1,2), ismatrix, 3, 2)

    ]
    for s1,s2,exp,r,c in mul_list:
        yield mul_shape, s1,s2,exp,r,c
    fail_list = [
        (Vector(4),Vector(3), TypeError),
        (Vector(4),Matrix(3,2), TypeError),
        (Matrix(3,2),Matrix(3,2), TypeError),
    ]
    for s1,s2,failure in fail_list:
        yield assert_raises, failure, mul_shape, s1,s2,None,None,None

# def test_stack():
#     stack_list = [
#         (Scalar(), Scalar(), Vector(2,1))
#     ]

def test_eval():
    s = Shape(['m','n','p','q',1,5,1,1,1])
    shape_dict = {'m':3, 'n':6, 'p':2, 'q':3}
    s.eval(shape_dict)
    assert(s.dimensions == [3,6,2,3,1,5])


