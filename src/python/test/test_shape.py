from scoop.qc_shape import Scalar, Vector, Matrix
from nose.tools import assert_raises
import operator

# tests for whether they are *exactly* these objects (or subclasses thereof)
def isvector_exact(x):
    return isinstance(x, Vector)

def isscalar_exact(x):
    return isinstance(x, Scalar)

def ismatrix_exact(x):
    return isinstance(x, Matrix)

shapes = [Scalar(), Vector('n'), Matrix('m','n')]

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
        (Scalar(),Scalar(), isscalar_exact, 1, 1),
        (Scalar(),Vector('n'), isvector_exact, 'n', 1),
        (Vector('n'),Scalar(), isvector_exact, 'n', 1),
        (Vector('m'),Vector('m'), isvector_exact, 'm', 1),
        (Scalar(), Matrix('a','b'), ismatrix_exact, 'a', 'b'),
        (Matrix('a','b'), Scalar(), ismatrix_exact, 'a', 'b'),
        (Matrix('a','b'),Matrix('a','b'), ismatrix_exact, 'a','b'),
    ]
    for s1,s2,exp,r,c in add_list:
        yield add_shape, s1,s2,exp,r,c
    fail_list = [
        (Vector('n'), Vector('m'), TypeError),
        (Matrix('a','b'),Vector('x'), TypeError),
        (Matrix('a','c'),Matrix('d','b'), TypeError), # disallowed for now
        (Vector('x'),Matrix('a','b'), TypeError)
    ]
    for s1,s2,failure in fail_list:
        yield assert_raises, failure, add_shape, s1,s2,None,None,None
            
def test_sub():
    sub_list = [
        (Scalar(),Scalar(), isscalar_exact, 1, 1),
        (Scalar(),Vector('n'), isvector_exact, 'n', 1),
        (Vector('n'),Scalar(), isvector_exact, 'n', 1),
        (Vector('m'),Vector('m'), isvector_exact, 'm', 1),
        (Scalar(), Matrix('a','b'), ismatrix_exact, 'a', 'b'),
        (Matrix('a','b'), Scalar(), ismatrix_exact, 'a', 'b'),
        (Matrix('a','b'),Matrix('a','b'), ismatrix_exact, 'a','b'),
    ]
    for s1,s2,exp,r,c in sub_list:
        yield sub_shape, s1,s2,exp,r,c
    fail_list = [
        (Vector('n'), Vector('m'), TypeError),
        (Matrix('a','b'),Vector('x'), TypeError),
        (Matrix('a','c'),Matrix('d','b'), TypeError), # disallowed for now
        (Vector('x'),Matrix('a','b'), TypeError)
    ]
    for s1,s2,failure in fail_list:
        yield assert_raises, failure, sub_shape, s1,s2,None,None,None


def test_negate():
    for s in shapes:
        yield negate_shape, s

def test_mul():
    mul_list = [
        (Scalar(),Scalar(), isscalar_exact, 1, 1),
        (Scalar(),Vector('x'), isvector_exact, 'x', 1),
        (Scalar(),Matrix('a','b'), ismatrix_exact, 'a', 'b'),
        (Vector('x'),Scalar(), isvector_exact, 'x', 1),
        (Matrix('a','x'),Vector('x'), isvector_exact, 'a', 1),
        (Matrix('a','b'),Scalar(), ismatrix_exact, 'a','b'),
        (Matrix('a','b'),Matrix('b','a'), ismatrix_exact, 'a', 'a'),
        (Vector('a'), Matrix(1,'b'), ismatrix_exact, 'a', 'b')
        
    ]
    for s1,s2,exp,r,c in mul_list:
        yield mul_shape, s1,s2,exp,r,c
    fail_list = [
        (Vector('x'),Vector('y'), TypeError),
        (Vector('x'),Matrix('a','b'), TypeError),
        (Matrix('a','b'),Matrix('a','b'), TypeError),
    ]
    for s1,s2,failure in fail_list:
        yield assert_raises, failure, mul_shape, s1,s2,None,None,None

# def test_stack():
#     stack_list = [
#         (Scalar(), Scalar(), Vector(2,1))
#     ]
    
    
    
    