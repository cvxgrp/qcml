from scoop.expression import Shape
from nose.tools import assert_raises
import operator

shapes = ['vector', 'scalar', 'matrix']

def create_shape(s):
    shape = Shape(str.upper(s))
    assert(shape.shape_str == str.upper(s))

def add_shape(s1,s2, exp):
    p1 = Shape(str.upper(s1))
    p2 = Shape(str.upper(s2))
    result = p1+p2
    assert(result.shape_str == str.upper(exp))
    
def negate_shape(s):
    p = Shape(str.upper(s))
    v = -p
    assert(v.shape_str == p.shape_str)

def mul_shape(s1,s2, exp):
    p1 = Shape(str.upper(s1))
    p2 = Shape(str.upper(s2))
    result = p1*p2
    assert(result.shape_str == str.upper(exp))

def test_add():
    add_list = [
        ('scalar','scalar', 'scalar'),
        ('scalar','vector', 'vector'),
        ('vector','scalar', 'vector'),
        ('vector','vector', 'vector'),
    ]
    for s1,s2,exp in add_list:
        yield add_shape, s1,s2,exp
    fail_list = [
        ('scalar','matrix', TypeError),
        ('matrix','scalar', TypeError),
        ('matrix','vector', TypeError),
        ('matrix','matrix', TypeError), # disallowed for now
        ('vector','matrix', TypeError)
    ]
    for s1,s2,failure in fail_list:
        yield assert_raises, failure, add_shape, s1,s2,''
            
def test_sub():
    s1 = Shape('SCALAR')
    s2 = Shape('SCALAR')
    yield assert_raises, Exception, operator.sub, s1, s2

def test_negate():
    for s in shapes:
        yield negate_shape, s

def test_mul():
    mul_list = [
        ('scalar','scalar', 'scalar'),
        ('scalar','vector', 'vector'),
        ('scalar','matrix', 'matrix'),
        ('vector','scalar', 'vector'),
        ('matrix','vector', 'vector'),
        ('matrix','scalar', 'matrix')
    ]
    for s1,s2,exp in mul_list:
        yield mul_shape, s1,s2,exp
    fail_list = [
        ('vector','vector', TypeError),
        ('vector','matrix', TypeError),
        ('matrix','matrix', TypeError),
    ]
    for s1,s2,failure in fail_list:
        yield assert_raises, failure, mul_shape, s1,s2,''
    

def equals(s1,s2):
    p1 = Shape(str.upper(s1))
    p2 = Shape(str.upper(s2))
    result = (p1 == p2)
    exp = (s1 == s2)
    assert(result == exp)

def not_equals(s1,s2):
    p1 = Shape(str.upper(s1))
    p2 = Shape(str.upper(s2))
    result = (p1 != p2)
    exp = (s1 != s2)
    assert(result == exp)
    
def test_shape_bools():
    for s1 in shapes:
        for s2 in shapes:
            yield equals, s1, s2
            yield not_equals, s1, s2
            
def test_shape_creation():
    for s in shapes:
        yield create_shape, s
    
    # attempt to create strange shape
    yield assert_raises, Exception, create_shape, 'tensor'
    yield assert_raises, Exception, create_shape, 'circle'
    yield assert_raises, Exception, create_shape, 'TREE'
    
    