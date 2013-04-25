from scoop.qc_sign import Positive, Negative, Neither
#from nose.tools import assert_raises
#import operator

signs = ['positive', 'negative', 'neither']

def create_sign(s):
    if s == 'positive':
        sign = Positive()
    elif s == 'negative':
        sign = Negative()
    else:
        sign = Neither()
    return sign
    
def make_sign(s):
    sign = create_sign(s)
    assert(str(sign) == s)

def add_sign(s1,s2, exp):
    p1 = create_sign(s1)
    p2 = create_sign(s2)
    result = p1+p2
    assert(str(result) == exp)

def sub_sign(s1,s2, exp):
    p1 = create_sign(s1)
    p2 = create_sign(s2)
    result = p1-p2
    assert(str(result) == exp)
    
def negate_sign(s, exp):
    p = create_sign(s)
    v = -p
    assert(str(v) == exp)

def mul_sign(s1,s2, exp):
    p1 = create_sign(s1)
    p2 = create_sign(s2)
    result = p1*p2
    assert(str(result) == exp)

def test_add():
    add_list = [
        ('positive','positive', 'positive'),
        ('positive','negative', 'neither'),
        ('positive','neither', 'neither'),
        ('negative','positive', 'neither'),
        ('negative','negative', 'negative'),
        ('negative','neither', 'neither'),
        ('neither','positive', 'neither'),
        ('neither','negative', 'neither'),
        ('neither','neither', 'neither'),
    ]
    for s1,s2,exp in add_list:
        yield add_sign, s1,s2,exp
            
def test_sub():
    sub_list = [
        ('positive','positive', 'neither'),
        ('positive','negative', 'positive'),
        ('positive','neither', 'neither'),
        ('negative','positive', 'negative'),
        ('negative','negative', 'neither'),
        ('negative','neither', 'neither'),
        ('neither','positive', 'neither'),
        ('neither','negative', 'neither'),
        ('neither','neither', 'neither'),
    ]
    for s1,s2,exp in sub_list:
        yield sub_sign, s1,s2,exp


def test_negate():
    add_list = [
        ('positive','negative'),
        ('negative','positive'),
        ('neither','neither')
    ]
    for s,exp in add_list:
        yield negate_sign, s, exp

def test_mul():
    mul_list = [
        ('positive','positive', 'positive'),
        ('positive','negative', 'negative'),
        ('positive','neither', 'neither'),
        ('negative','positive', 'negative'),
        ('negative','negative', 'positive'),
        ('negative','neither', 'neither'),
        ('neither','positive', 'neither'),
        ('neither','negative', 'neither'),
        ('neither','neither', 'neither'),
    ]
    for s1,s2,exp in mul_list:
        yield mul_sign, s1,s2,exp
    

# def equals(s1,s2):
#     p1 = Sign(str.upper(s1))
#     p2 = Sign(str.upper(s2))
#     result = (p1 == p2)
#     exp = (s1 == s2)
#     assert(result == exp)
# 
# def not_equals(s1,s2):
#     p1 = Sign(str.upper(s1))
#     p2 = Sign(str.upper(s2))
#     result = (p1 != p2)
#     exp = (s1 != s2)
#     assert(result == exp)
#     
# def test_sign_bools():
#     for s1 in signs:
#         for s2 in signs:
#             yield equals, s1, s2
#             yield not_equals, s1, s2
            
def test_sign_creation():
    for s in signs:
        yield make_sign, s
    
    
    