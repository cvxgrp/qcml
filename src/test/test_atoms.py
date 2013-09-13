# this tests the atoms by feeding them constant arguments
# invokes CVXOPT and checks that objval is within tolerance to expected
# constant value
from qcml import QCML
import numpy
from nose.tools import assert_raises

TOL = 1e-3

v = numpy.array([-1.,2.,-2.])
convex_template = "minimize %s"
concave_template = "maximize %s"

convex_list = [
    ("abs(-5)", 5),
    ("abs(5)", 5),
    ("huber(0.5)", 0.25),
    ("huber(-1.5)", 2),
    ("inv_pos(5)", 0.2),
    ("max(2,5,1)", 5),
    ("norm_inf(v)", 2),
    ("norm(v)", 3),
    ("norm2(v)", 3),
    ("norm1(v)", 5),
    ("pos(8)", 8),
    ("pos(-3)", 0),
    ("neg(-3)", 3),
    ("neg(3)", 0),
    ("pow_rat(4,1,1)", 4),
    ("pow_rat(2,2,1)", 4),
    ("pow_rat(4,2,2)", 4),
    ("pow_rat(2,3,1)", 8),
    ("pow_rat(4,3,2)", 8),
    ("pow_rat(4,3,3)", 4),
    ("pow_rat(2,4,1)", 16),
    ("pow_rat(4,4,2)", 16),
    ("pow_rat(8,4,3)", 16),
    ("pow_rat(8,4,4)", 8),
    ("quad_over_lin(v, 2)", 4.5),
    ("square_over_lin(2,4)", 1),
    ("square(3)", 9)
]

concave_list = [
    ("geo_mean(4,1)", 2),
    ("geo_mean(2,2)", 2),
    ("min(3,4,-1)", -1),
    ("pow_rat(4,1,2)", 2),
    ("pow_rat(8,1,3)", 2),
    ("pow_rat(16,1,4)",2),
    ("pow_rat(8,2,3)", 4),
    ("pow_rat(4,2,4)", 2),
    ("pow_rat(16,3,4)",8),
    ("sqrt(2)", 1.414213562373095)
]

# OOD tests
# inv_pos(-3) = Inf

def run_atom(template, obj, obj_val):
    #print template % obj
    p = QCML(debug=True)
    p.parse("""
        parameter v(3)
        %s
    """ % (template % obj))
    solution = p.solve(params={'v': v})

    print solution['objval']
    print obj_val
    assert( abs(solution['objval'] - obj_val) <= TOL )

def test_atom():
    for obj, obj_val in convex_list:
        yield run_atom, convex_template, obj, obj_val

    for obj, obj_val in concave_list:
        yield run_atom, concave_template, obj, obj_val

