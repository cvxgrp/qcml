from qcml.qc_ast import Convex, Concave, Affine, Nonconvex
#from nose.tools import assert_raises
#import operator

vexities = ['convex', 'concave', 'affine', 'nonconvex']


def create_vex(s):
    if s == 'convex':
        sign = Convex()
    elif s == 'concave':
        sign = Concave()
    elif s == 'affine':
        sign = Affine()
    else:
        sign = Nonconvex()
    return sign

def make_vex(s):
    sign = create_vex(s)
    assert(str(sign) == s)

def add_vex(s1,s2, exp):
    p1 = create_vex(s1)
    p2 = create_vex(s2)
    result = p1+p2
    assert(str(result) == exp)

def sub_vex(s1,s2, exp):
    p1 = create_vex(s1)
    p2 = create_vex(s2)
    result = p1-p2
    assert(str(result) == exp)

def negate_vex(s, exp):
    p = create_vex(s)
    v = -p
    assert(str(v) == exp)

def test_add():
    add_list = [
        ('convex','convex', 'convex'),
        ('convex','concave', 'nonconvex'),
        ('convex','affine', 'convex'),
        ('convex','nonconvex', 'nonconvex'),
        ('concave','convex', 'nonconvex'),
        ('concave','concave', 'concave'),
        ('concave','affine', 'concave'),
        ('concave','nonconvex', 'nonconvex'),
        ('affine','convex', 'convex'),
        ('affine','concave', 'concave'),
        ('affine','nonconvex', 'nonconvex'),
        ('affine','affine', 'affine'),
        ('nonconvex','convex', 'nonconvex'),
        ('nonconvex','concave', 'nonconvex'),
        ('nonconvex','nonconvex', 'nonconvex'),
        ('nonconvex','affine', 'nonconvex'),
    ]
    for s1,s2,exp in add_list:
        yield add_vex, s1,s2,exp

def test_sub():
    sub_list = [
        ('convex','convex', 'nonconvex'),
        ('convex','concave', 'convex'),
        ('convex','affine', 'convex'),
        ('convex','nonconvex', 'nonconvex'),
        ('concave','convex', 'concave'),
        ('concave','concave', 'nonconvex'),
        ('concave','affine', 'concave'),
        ('concave','nonconvex', 'nonconvex'),
        ('affine','convex', 'concave'),
        ('affine','concave', 'convex'),
        ('affine','nonconvex', 'nonconvex'),
        ('affine','affine', 'affine'),
        ('nonconvex','convex', 'nonconvex'),
        ('nonconvex','concave', 'nonconvex'),
        ('nonconvex','nonconvex', 'nonconvex'),
        ('nonconvex','affine', 'nonconvex'),
    ]
    for s1,s2,exp in sub_list:
        yield sub_vex, s1,s2,exp

# def test_sub():
#     s1 = Sign('POSITIVE')
#     s2 = Sign('POSITIVE')
#     yield assert_raises, Exception, operator.sub, s1, s2

def test_negate():
    add_list = [
        ('convex','concave'),
        ('concave','convex'),
        ('affine','affine'),
        ('nonconvex', 'nonconvex')
    ]
    for s,exp in add_list:
        yield negate_vex, s, exp


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

def test_vex_creation():
    for s in vexities:
        yield make_vex, s


