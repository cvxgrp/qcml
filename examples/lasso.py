#!/usr/bin/env python
from qcml import QCML
import numpy as np
from numpy.random import randn
import scipy.sparse as sparse
import subprocess, os, sys, platform

if __name__ == '__main__':

    if len(sys.argv) < 2:
        print "Please provide a path to ECOS solver."
        sys.exit(0)
    else:
        ECOS_PATH = sys.argv[1]

    print "Creating data."
    n = 1000    # number of features
    m = 100     # number of examples
    A = randn(m,n)
    b = randn(m)
    gamma = 1


    print "Creating lasso problem."

    # a QCML model is specified by strings
    #   the parser parses each model line by line and builds an internal
    #   representation of an SOCP
    s = """
    dimensions m n
    variable x(n)
    parameters A(m,n) b(m)
    parameter gamma positive
    minimize (square(norm(A*x - b)) + gamma*norm1(x))
    """
    print s

    raw_input("press ENTER to parse....")
    p = QCML(debug=True)
    p.parse(s)

    raw_input("press ENTER to canonicalize....")
    p.canonicalize()

    raw_input("press ENTER to solve the problem....")
    res = p.solve()

    raw_input("press ENTER to generate C code and save it....")
    p.codegen("C")
    p.save("lasso")

    raw_input("press ENTER to write the test C program....")
    c_template = """
#include <stdio.h>
#include "lasso.h"
#include "ecos.h"

int read_file(const char * file, void *x, size_t size, size_t num)
{
  FILE *f;
  f = fopen(file, "rb");
  if (!f)
  {
    printf("Unable to open %%s!\\n", file);
    return 1;
  }
  fread(x, size, num, f);
  fclose(f);
  return 0;
}

int main(int argc, char **argv)
{
  double Av[%(Asize)d], b[%(bsize)d];
  long Ai[%(Asize)d], Aj[%(Asize)d];
  
  // create parameter struct
  lasso_params p;
  lasso_dims d;
  qc_matrix A;

  int res1 = read_file("Av", Av, sizeof(double), %(Asize)d);
  int res2 = read_file("Ai", Ai, sizeof(long), %(Asize)d);
  int res3 = read_file("Aj", Aj, sizeof(long), %(Asize)d);
  int res4 = read_file("b", b, sizeof(double), %(bsize)d);

  if (res1 || res2 || res3 || res4)
  {
    printf("Problem reading data.\\n");
    return 1;
  }

  A.v = Av; A.i = Ai; A.j = Aj; A.nnz = %(Asize)d;
  A.m = %(m)d; A.n = %(n)d;

  p.A = &A;
  p.b = b;
  p.gamma = %(gamma)f;
  
  // assign dims, if any

  // stuff the matrices
  qc_socp *data = qc_lasso2socp(&p, &d);

  // run ecos and solve it
  pwork *mywork = ECOS_setup(data->n, data->m, data->p,
      data->l, data->nsoc, data->q,
      data->Gx, data->Gp, data->Gi,
      data->Ax, data->Ap, data->Ai,
      data->c, data->h, data->b);

  if (mywork)
  {
      ECOS_solve(mywork);
      printf("Objective value at termination of C program is %%f\\n", mywork->info->pcost);
      ECOS_cleanup(mywork, 0);
  }

  qc_socp_free(data);
  return 0;
}
"""

    A_coo = sparse.coo_matrix(A)
    A_coo.data.tofile("lasso/Av")
    A_coo.row.astype("int64").tofile("lasso/Ai")
    A_coo.col.astype("int64").tofile("lasso/Aj")

    b.tofile("lasso/b")

    constants = {'Asize': A.size, 'bsize': b.size, 'm': m, 'n': n, 'gamma': gamma}

    with open("lasso/lasso_main.c", "w") as f:
        f.write(c_template % constants)

    os.chdir("lasso")
    print "Running make...."
    subprocess.call(["make"])
    if platform.system() == 'Linux':
        cmd = ["cc", "-O3", "lasso_main.c", 
                "-L%s" % ECOS_PATH, 
                "-I%s/include" % ECOS_PATH, "-I%s/external/SuiteSparse_config" % ECOS_PATH,
                "-lecos", "-lm", "-lrt", "lasso.o", "qcml_utils.o", "-o","lasso"]
    else:
        cmd = ["cc", "-O3", "lasso_main.c", 
                "-L%s" % ECOS_PATH, 
                "-I%s/include" % ECOS_PATH, "-I%s/external/SuiteSparse_config" % ECOS_PATH,
                "-lecos", "-lm", "lasso.o", "qcml_utils.o", "-o","lasso"]
    print ' '.join(cmd)
    subprocess.call(cmd)

    print
    raw_input("press ENTER to run C program....")
    subprocess.call(["./lasso"])
    #os.chdir("..")

    print "Verify that the reported objective in C is %f" % res['objval']
