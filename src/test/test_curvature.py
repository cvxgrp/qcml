from .. properties.curvature import Constant, Convex, Concave, Affine, Nonconvex
#from nose.tools import assert_raises
#import operator

curvatures = ['constant', 'convex', 'concave', 'affine', 'nonconvex']


def create_curvature(s):
    if s == 'constant': return Constant()
    if s == 'convex': return Convex()
    if s == 'concave': return Concave()
    if s == 'affine': return Affine()
    return Nonconvex()

def make_curvature(s):
    vex = create_curvature(s)
    assert(str(vex) == s)

def add_curvature(s1,s2, exp):
    p1 = create_curvature(s1)
    p2 = create_curvature(s2)
    result = p1+p2
    assert(str(result) == exp)

def sub_curvature(s1,s2, exp):
    p1 = create_curvature(s1)
    p2 = create_curvature(s2)
    result = p1-p2
    assert(str(result) == exp)

def negate_curvature(s, exp):
    p = create_curvature(s)
    v = -p
    assert(str(v) == exp)

def test_add():
    add_list = [
        ('constant', 'constant', 'constant'),
        ('constant', 'affine', 'affine'),
        ('constant', 'convex', 'convex'),
        ('constant', 'concave', 'concave'),
        ('constant', 'nonconvex', 'nonconvex'),
        ('convex','convex', 'convex'),
        ('convex','concave', 'nonconvex'),
        ('convex','affine', 'convex'),
        ('convex','constant','convex'),
        ('convex','nonconvex', 'nonconvex'),
        ('concave','convex', 'nonconvex'),
        ('concave','concave', 'concave'),
        ('concave','affine', 'concave'),
        ('concave','constant','concave'),
        ('concave','nonconvex', 'nonconvex'),
        ('affine','convex', 'convex'),
        ('affine','concave', 'concave'),
        ('affine','constant', 'affine'),
        ('affine','nonconvex', 'nonconvex'),
        ('affine','affine', 'affine'),
        ('nonconvex','convex', 'nonconvex'),
        ('nonconvex','concave', 'nonconvex'),
        ('nonconvex','nonconvex', 'nonconvex'),
        ('nonconvex','affine', 'nonconvex'),
        ('nonconvex','constant', 'nonconvex'),
    ]
    for s1,s2,exp in add_list:
        yield add_curvature, s1,s2,exp

def test_sub():
    sub_list = [
        ('constant', 'constant', 'constant'),
        ('constant', 'affine', 'affine'),
        ('constant', 'convex', 'concave'),
        ('constant', 'concave', 'convex'),
        ('constant', 'nonconvex', 'nonconvex'),
        ('convex','convex', 'nonconvex'),
        ('convex','concave', 'convex'),
        ('convex','affine', 'convex'),
        ('convex','constant', 'convex'),
        ('convex','nonconvex', 'nonconvex'),
        ('concave','convex', 'concave'),
        ('concave','concave', 'nonconvex'),
        ('concave','affine', 'concave'),
        ('concave','constant', 'concave'),
        ('concave','nonconvex', 'nonconvex'),
        ('affine','convex', 'concave'),
        ('affine','concave', 'convex'),
        ('affine','nonconvex', 'nonconvex'),
        ('affine','affine', 'affine'),
        ('affine','constant', 'affine'),
        ('nonconvex','convex', 'nonconvex'),
        ('nonconvex','concave', 'nonconvex'),
        ('nonconvex','nonconvex', 'nonconvex'),
        ('nonconvex','affine', 'nonconvex'),
        ('nonconvex','constant', 'nonconvex'),
    ]
    for s1,s2,exp in sub_list:
        yield sub_curvature, s1,s2,exp

# def test_sub():
#     s1 = Sign('POSITIVE')
#     s2 = Sign('POSITIVE')
#     yield assert_raises, Exception, operator.sub, s1, s2

def test_negate():
    add_list = [
        ('convex','concave'),
        ('concave','convex'),
        ('affine','affine'),
        ('constant', 'constant'),
        ('nonconvex', 'nonconvex')
    ]
    for s,exp in add_list:
        yield negate_curvature, s, exp


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

def test_curvature_creation():
    for s in curvatures:
        yield make_curvature, s


