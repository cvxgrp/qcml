"""
QCML stands for the quadratic cone modeling language. It is a modeling
language for generating code compatible with convex, cone solvers, typically
interior-point methods. It is able to target C, Matlab, and Python.

QCML targets *second-order cone* optimization problems (SOCP). It separates
the parsing and canonicalization phase from the code generation and solve
phase. This allows the use of a unified (domain-specific) language in the
front end to target different use cases.

For instance, a simple portfolio optimization problem can be specified as a
Python string as follows:

    dimensions m n

    variable x(n)
    parameter mu(n)
    parameter gamma positive
    parameter F(n,m)
    parameter D(n,n)
    maximize (mu'*x - gamma*(square(norm(F'*x)) + square(norm(D*x))))
        sum(x) == 1
        x >= 0

Our tool parses the problem and rewrites it, after which it can generate
Python code or external source code. The basic workflow is as follows
(assuming `s` stores a problem specification).

    p.parse(s)
    p.canonicalize()
    p.dims = {'m': m, 'n': n}
    p.codegen("python")
    socp_data = p.prob2socp({'gamma':1,'F':F,'D':D})
    sol = ecos.solve(**socp_data)
    my_vars = p.socp2prob(sol['x'])

We will walk through each line:

1. `parse` the optimization problem and check that it is convex
2. `canonicalize` the problem by symbolically converting it to a second-order
    cone program
3. assign the `dims` of the problem
4. generate `python` code for converting parameters into SOCP data and for
   converting the SOCP solution into the problem variables
5. call a solver with the SOCP data structure
6. recover the original solution

For rapid prototyping, we provide the convenience function:

    solution = p.solve()

This functions wraps all six steps above into a single call and
assumes that all parameters and dimensions are defined in the local
namespace.

Finally, you can call

    p.codegen("C", name="myprob")

which will produce a directory called `myprob` with five files:

* `myprob.h` -- header file for the `prob2socp` and `socp2prob` functions
* `myprob.c` -- source code / implementation of the two functions
* `qc_utils.h` -- defines static matrices and basic data structures
* `qc_utils.c` -- source code for matrices and data structures
* `Makefile` -- sample Makefile to compile the `.o` files

You can include the header and source files with any project, although you
must supply your own solver. The code simply stuffs the matrices for you; you
are still responsible for using the proper solver and linking it. An example
of how this might work is in `examples/lasso.py`.

The `qc_utils` files are static; meaning, if you have multiple sources you
wish to use in a project, you only need one copy of `qc_utils.h` and
`qc_utils.c`.

The generated code uses portions of CSparse, which is LGPL. Although QCML is
BSD, the generated code is LGPL for this reason.
"""

# attach the code generator to the Scoop class
# from types import MethodType
# Scoop.generate = MethodType(g1.generate, None, Scoop)
from . qc_lang import QCML

# TODO: this really shouldn't be here
from . qc_rewrite import QCRewriter
