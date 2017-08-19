"""
Tests that an expression like A*B*x is properly handled in Python and in C.
(Sorry, no love for Matlab.)

Should be able to run generated Makefile without any errors (except warnings)
and execute C code.
"""
import numpy as np
from . check_ecos import make_and_execute_ecos_solve

mult = """
variable x(2)
parameters A(3,2) B(3,2)
parameters c(3) d(3)
minimize 2*c'*A*x
x == B'*d
"""

A = [[1,2],[3,4],[5,6]]
B = [[1,4], [2,5], [3,6]]
c = [1,2,3]
d = [4,5,6]

solution = 2*(np.matrix(c)*np.matrix(A)*np.matrix(B).T*np.matrix(d).T)[0]


def properly_solves(lang):
    from .. qc_lang import QCML
    p = QCML(debug=True)
    p.parse(mult)
    p.canonicalize()
    if lang == "C":
        p.codegen(lang)
        p.save("test_problem")

        c_test_code = """
#include "test_problem.h"
#include "ecos.h"

int main(int argc, char **argv) {
    double Adata[6] = {1,2,3,4,5,6};
    double Bdata[6] = {1,2,3,4,5,6};

    long Ai[6] = {0,0,1,1,2,2};
    long Aj[6] = {0,1,0,1,0,1};

    long Bj[6] = {0,0,0,1,1,1};
    long Bi[6] = {0,1,2,0,1,2};

    double c[3] = {1,2,3};
    double d[3] = {4,5,6};

    qc_matrix A;
    qc_matrix B;

    A.v = Adata; A.i = Ai; A.j = Aj; A.nnz = 6;
    A.m = 3; A.n = 2;

    B.v = Bdata; B.i = Bi; B.j = Bj; B.nnz = 6;
    B.m = 3; B.n = 2;

    test_problem_params p;

    p.A = &A;
    p.B = &B;
    p.c = c;
    p.d = d;

	qc_socp *data = qc_test_problem2socp(&p, NULL);

	// run ecos and solve it
	pwork *mywork = ECOS_setup(data->n, data->m, data->p,
		data->l, data->nsoc, data->q, 0,
		data->Gx, data->Gp, data->Gi,
		data->Ax, data->Ap, data->Ai,
		data->c, data->h, data->b);

	if (mywork)
	{
		ECOS_solve(mywork);
		printf("Objective value at termination of C program is %f\\n", mywork->info->pcost);
		ECOS_cleanup(mywork, 0);
	}
	qc_socp_free(data);

    return 0;
}
"""
        objval, _ = make_and_execute_ecos_solve("test_problem", c_test_code)
        assert objval == solution
    else:
        p.codegen(lang)
        socp_data = p.prob2socp({'A': np.array(A), 'B': np.array(B), 'c': np.array(c), 'd': np.array(d)})
        print socp_data

        import ecos
        sol = ecos.solve(**socp_data)
        print sol
        assert abs(sol['info']['pcost'] - solution) < 1e-6

def test_parse_and_compiles():
    yield properly_solves, "python"
    yield properly_solves, "C"
