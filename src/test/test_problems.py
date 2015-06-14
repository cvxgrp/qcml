import numpy as np
from . check_ecos import make_and_execute_ecos_solve
from .. qc_lang import QCML

sum_lp = """
variable x(2)
dual variable y
parameter c
minimize sum(2*x) + c + 1
y : x >= 0
"""

sum_mat_lp = """
variable x(2)
dual variable y
parameter D(2,2)
parameter c
minimize sum(D*x) + c + 2
y : x >= 0
"""

sum_mat_lp_with_scale = """
variable x(2)
dual variables u v
parameter D(2,2)
parameter c
minimize sum(2*D*x) + c + 3
u : x >= 0
v : 2*D*x + x == 3
"""

mix_quad_affine_constr = """
variable x(2)
dual variable y
parameter D(2,2)
parameter b(2)
minimize sum(x)
y : square(norm(D*x)) - 2*b'*x <= 0
"""

github_issue_45 = """
parameter b(2)
variable x(2)
dual variable y
minimize( norm(x) )
y : b'*x==1
"""

multi_parameters = """
variable x(2)
parameter D(2,2)
parameter b(2)
parameter c
minimize sum(square((D*x  - b'*x + c)))
"""

scalar_times_vector_parameter = """
variable x(2)
parameter b(2)

minimize abs(2*b'*x)
"""

def python_parse_and_solve(prob, expected_objval, dual1=None, dual2=None):
    p = QCML(debug=True)
    p.parse(prob)
    D = np.matrix([[0.1, 0], [0, 3.1]])
    c = 5
    b = np.matrix([[1.0],[2.0]])
    sol = p.solve()
    print "Expecting", sol['info']['pcost']
    assert abs(sol['info']['pcost'] - expected_objval) < 1e-6
    if dual1 is not None and dual2 is not None:
        print "Expecting", dual1, "but got", sol['u']
        assert np.linalg.norm(sol['u'] - dual1) < 1e-6
        print "Expecting", dual2, "but got", sol['v']
        assert np.linalg.norm(sol['v'] - dual2) < 1e-6
    elif dual1 is not None:
        print "Expecting", dual1, "but got", sol['y']
        assert np.linalg.norm(sol['y'] - dual1) < 1e-6
    return p

def C_parse_and_codegen(prob):
    p = QCML(debug=True)
    p.parse(prob)
    p.canonicalize()
    p.codegen("C")
    return p

def C_parse_and_solve(prob, expected_objval, dual1=None, dual2=None):
    p = C_parse_and_codegen(prob)
    print p.program

    p.save("test_problem")

    def print_dual():
        line = ""
        if dual1 is not None and dual2 is not None:
            float_format = ','.join(["%f"]*dual1.size)
            floats = ','.join("v.u[%d]" % i for i in range(dual1.size))
            line1 = """printf("Dual: %s\\n", %s);""" % (float_format, floats)
            float_format = ','.join(["%f"]*dual1.size)
            floats = ','.join("v.v[%d]" % i for i in range(dual1.size))
            line2 = """printf("Dual: %s\\n", %s);""" % (float_format, floats)
            line = " "*8 + line1 + "\n" + " "*8 + line2
        elif dual1 is not None:
            float_format = ','.join(["%f"]*dual1.size)
            floats = ','.join("v.y[%d]" % i for i in range(dual1.size))
            line = " "*8 + """printf("Dual: %s\\n", %s);""" % (float_format, floats)

        return line

    c_test_code = """
#include "test_problem.h"
#include "ecos.h"

int main(int argc, char **argv) {
""" + \
("""
    double Ddata[2] = {0.1,3.1};

    long Di[2] = {0,1};
    long Dj[2] = {0,1};
    qc_matrix D;
""" if 'D' in p.program.parameters else "") + \
("""
    double bdata[2] = {-1.2, 3.1};
""" if 'b' in p.program.parameters else "") + \
("""
    D.v = Ddata; D.i = Di; D.j = Dj; D.nnz = 2;
    D.m = 2; D.n = 2;
""" if 'D' in p.program.parameters else "") + \
"""
    test_problem_params p;
    test_problem_vars v;
    test_problem_dims dims;
""" + ("p.D = &D;" if 'D' in p.program.parameters else "") \
    + ("p.c = 5.0;" if 'c' in p.program.parameters else "") \
    + ("p.b = bdata;" if 'b' in p.program.parameters else "") + \
"""
    qc_socp *data = qc_test_problem2socp(&p, NULL);

    /* run ecos and solve it */
    pwork *mywork = ECOS_setup(data->n, data->m, data->p,
        data->l, data->nsoc, data->q,
        data->Gx, data->Gp, data->Gi,
        data->Ax, data->Ap, data->Ai,
        data->c, data->h, data->b);

    if (mywork)
    {
        ECOS_solve(mywork);
        printf("Objective value at termination of C program is %f\\n", mywork->info->pcost);
        qc_socp2test_problem(mywork->x, mywork->y, mywork->z, &v, &dims);
""" + print_dual() + \
"""
        ECOS_cleanup(mywork, 0);
    }
    qc_socp_free(data);

    return 0;
}
"""
    objval, duals = make_and_execute_ecos_solve("test_problem", c_test_code)
    print objval
    print print_dual()

    assert abs(objval - expected_objval) < 1e-6
    if dual1 is not None and dual2 is not None:
        print "Expecting", dual1, "but got", duals[0]
        assert np.linalg.norm(np.array(duals[0]) - dual1) < 1e-6
        print "Expecting", dual2, "but got", duals[1]
        assert np.linalg.norm(np.array(duals[1]) - dual2) < 1e-6
    elif dual1 is not None:
        print "Expecting", dual1, "but got", duals[0]
        assert np.linalg.norm(np.array(duals[0]) - dual1) < 1e-6


def test_solves():
    yield python_parse_and_solve, sum_lp, 0, np.array([2, 2])
    yield C_parse_and_codegen, sum_lp

    yield python_parse_and_solve, sum_mat_lp, 0, np.array([0.1, 3.1])
    yield C_parse_and_codegen, sum_mat_lp
    yield C_parse_and_solve, sum_mat_lp, 0, np.array([0.1, 3.1])

    yield python_parse_and_solve, sum_mat_lp_with_scale, 3.08333333, np.array([0,0]), np.array([-0.16666666,-0.86111111])
    yield C_parse_and_codegen, sum_mat_lp_with_scale
    yield C_parse_and_solve, sum_mat_lp_with_scale, 3.083333333, np.array([0,0]), np.array([-0.16666666,-0.86111111])

    yield python_parse_and_solve, mix_quad_affine_constr, -0.0519076361544, np.array([0.49922209012352059])
    yield python_parse_and_solve, github_issue_45, 0.447213582782, np.array([-0.4472135906730919])
    yield python_parse_and_solve, multi_parameters, 0

    yield python_parse_and_solve, scalar_times_vector_parameter, 0
    yield C_parse_and_codegen, scalar_times_vector_parameter
    yield C_parse_and_solve, scalar_times_vector_parameter, 0
