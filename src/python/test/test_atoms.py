# this tests the atoms by feeding them constant arguments
# invokes CVXOPT and checks that objval is within tolerance to expected
# constant value
from qcml import QCML
import cvxopt
from nose.tools import assert_raises

TOL = 1e-6

v = cvxopt.matrix([-1,2,-2], tc='d')
convex_template = "minimize %s"
concave_template = "maximize %s"

convex_list = [
    ("abs(-5)", 5),
    ("abs(5)", 5),
    ("huber(0.5)", 0.25),
    ("huber(-1.5)", 2),
    ("inv_pos(5)", 0.2),
    ("inv_pos(-3)", 0),
    ("max(2,5,1)", 5),
    ("norm_inf(v)", 2),
    ("norm(v)", 3),
    ("norm2(v)", 3),
    ("norm1(v)", 5),
    ("pos(8)", 8),
    ("pos(-2)", 0),
    ("pos(v)", 2),
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
    ("neg(8)", 0),
    ("neg(-3)", 3),
    ("neg(v)", 2),
    ("pow_rat(4,1,2)", 2),
    ("pow_rat(8,1,3)", 2),
    ("pow_rat(16,1,4)",2),
    ("pow_rat(8,2,3)", 4),
    ("pow_rat(4,2,4)", 2),
    ("pow_rat(16,3,4)",8),
    ("sqrt(2)", 1.414213562373095)
]

def run_atom(template, obj, obj_val):
    #print template % obj
    with QCML(debug=True, local_dict={'v': v}) as p:
        p.parse("""
            parameter v(3)
            %s
        """ % (template % obj))

    assert( False ) #abs(self.solution['primal objective'] - obj_val) <= TOL )

def test_atom():
    for obj, obj_val in convex_list:
        yield run_atom, convex_template, obj, obj_val

    for obj, obj_val in concave_list:
        yield run_atom, concave_template, obj, obj_val


#
#     p.codegen("pdos")
#     p.prettyprint(True)
#     s = p.solver(m=m,n=n,X=X,Y=Y,gamma=gamma)
#
#     p.codegen("matlab",cone_size=3,m=100,n=10)
#     p.prettyprint()



    # if y:
    #     #y.show()
    #     visit.visit(y)
    #
    #     print y
    #     # TODO: before codegen, need to check DCP compliance and that objective is scalar
    #     codegen = ECOSCodegen(visit.replaced_expressions())
    #     codegen.visit(y)
    #     codegen.prettyprint(True)
    #
    #     f = codegen.codegen()
    #     #s = f(m=1,n=1,A=1,b=1)#,gamma=0.1)
    #     s = f(m=m,n=n,X=X,Y=Y,gamma=gamma)
    #
    #     print s
    #     print s['a']
    #     print s['b']
