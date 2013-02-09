from scoop.expression import Sign
from nose.tools import assert_raises
import operator

signs = ['positive', 'negative', 'unknown']

def create_sign(s):
    sign = Sign(str.upper(s))
    assert(sign.sign_str == str.upper(s))

def add_sign(s1,s2, exp):
    p1 = Sign(str.upper(s1))
    p2 = Sign(str.upper(s2))
    result = p1+p2
    assert(result.sign_str == str.upper(exp))
    
def negate_sign(s, exp):
    p = Sign(str.upper(s))
    v = -p
    assert(v.sign_str == str.upper(exp))

def mul_sign(s1,s2, exp):
    p1 = Sign(str.upper(s1))
    p2 = Sign(str.upper(s2))
    result = p1*p2
    assert(result.sign_str == str.upper(exp))

def test_add():
    add_list = [
        ('positive','positive', 'positive'),
        ('positive','negative', 'unknown'),
        ('positive','unknown', 'unknown'),
        ('negative','positive', 'unknown'),
        ('negative','negative', 'negative'),
        ('negative','unknown', 'unknown'),
        ('unknown','positive', 'unknown'),
        ('unknown','negative', 'unknown'),
        ('unknown','unknown', 'unknown'),
    ]
    for s1,s2,exp in add_list:
        yield add_sign, s1,s2,exp
            
def test_sub():
    s1 = Sign('POSITIVE')
    s2 = Sign('POSITIVE')
    yield assert_raises, Exception, operator.sub, s1, s2

def test_negate():
    add_list = [
        ('positive','negative'),
        ('negative','positive'),
        ('unknown','unknown')
    ]
    for s,exp in add_list:
        yield negate_sign, s, exp

def test_mul():
    mul_list = [
        ('positive','positive', 'positive'),
        ('positive','negative', 'negative'),
        ('positive','unknown', 'unknown'),
        ('negative','positive', 'negative'),
        ('negative','negative', 'positive'),
        ('negative','unknown', 'unknown'),
        ('unknown','positive', 'unknown'),
        ('unknown','negative', 'unknown'),
        ('unknown','unknown', 'unknown'),
    ]
    for s1,s2,exp in mul_list:
        yield mul_sign, s1,s2,exp
    

def equals(s1,s2):
    p1 = Sign(str.upper(s1))
    p2 = Sign(str.upper(s2))
    result = (p1 == p2)
    exp = (s1 == s2)
    assert(result == exp)

def not_equals(s1,s2):
    p1 = Sign(str.upper(s1))
    p2 = Sign(str.upper(s2))
    result = (p1 != p2)
    exp = (s1 != s2)
    assert(result == exp)
    
def test_sign_bools():
    for s1 in signs:
        for s2 in signs:
            yield equals, s1, s2
            yield not_equals, s1, s2
            
def test_sign_creation():
    for s in signs:
        yield create_sign, s
    
    # attempt to create strange sign
    yield assert_raises, Exception, create_sign, 'reallyneg'
    yield assert_raises, Exception, create_sign, 'zero'
    yield assert_raises, Exception, create_sign, 'infinty'
    
    