"""
Tests that an expression like A*B*x is properly handled in Python and in C.
(Sorry, no love for Matlab.)

Should be able to run generated Makefile without any errors (except warnings)
and execute C code.
"""
import os, shutil, subprocess, platform, re
import numpy as np

FILE_PATH = os.path.dirname(__file__)
DEFAULT = os.path.join(FILE_PATH, "../../scripts/ecos")
ECOS_PATH = os.environ.get("ECOS_PATH")
ECOS_PATH = ECOS_PATH if ECOS_PATH else DEFAULT

mult = """
variable x(2)
parameters A(3,2) B(2,3)
parameters c(3) d(3)
minimize c'*A*x
x == B*d
"""

A = [[1,2],[3,4],[5,6]]
B = [[1,2,3], [4,5,6]]
c = [1,2,3]
d = [4,5,6]

solution = (np.matrix(c)*np.matrix(A)*np.matrix(B)*np.matrix(d).T)[0]


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

    long Bi[6] = {0,0,0,1,1,1};
    long Bj[6] = {0,1,2,0,1,2};

    double c[3] = {1,2,3};
    double d[3] = {4,5,6};

    qc_matrix A;
    qc_matrix B;

    A.v = Adata; A.i = Ai; A.j = Aj; A.nnz = 6;
    A.m = 3; A.n = 2;

    B.v = Bdata; B.i = Bi; B.j = Bj; B.nnz = 6;
    B.m = 2; B.n = 3;

    test_problem_params p;

    p.A = &A;
    p.B = &B;
    p.c = c;
    p.d = d;

	qc_socp *data = qc_test_problem2socp(&p, NULL);

	// run ecos and solve it
	pwork *mywork = ECOS_setup(data->n, data->m, data->p,
		data->l, data->nsoc, data->q,
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
        with open("test_problem/main.c", "w") as f:
            f.write(c_test_code)

        os.chdir("test_problem")
        print "Running make...."
        subprocess.check_output(["make"])
        if platform.system() == 'Linux':
            cmd = ["cc", "-O3", "main.c",
                    "-L%s" % ECOS_PATH,
                    "-I%s/include" % ECOS_PATH, "-I%s/external/SuiteSparse_config" % ECOS_PATH,
                    "-lecos", "-lm", "-lrt", "test_problem.o", "qcml_utils.o", "-o","main"]
        else:
            cmd = ["cc", "-O3", "main.c",
                    "-L%s" % ECOS_PATH,
                    "-I%s/include" % ECOS_PATH, "-I%s/external/SuiteSparse_config" % ECOS_PATH,
                    "-lecos", "-lm", "test_problem.o", "qcml_utils.o", "-o","main"]
        print ' '.join(cmd)
        try:
            subprocess.check_output(cmd)
        except subprocess.CalledProcessError:
            print ""
            print "This test fails because it cannot find ECOS,"
            print "please set your ECOS_PATH environment variable."
            print ""
            print "    export ECOS_PATH=/PATH/TO/ECOS"
            print ""
            raise

        try:
            output = subprocess.check_output(["./main"])
        except:
            print ""
            print "There is a bug in the compiled C code."
            print ""
            raise
        objval = re.findall(r'Objective value at termination of C program is (\d+\.\d*)', output)
        os.chdir("..")
        shutil.rmtree("%s/test_problem" % os.getcwd())

        assert len(objval) == 1
        assert float(objval[0]) == solution
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
