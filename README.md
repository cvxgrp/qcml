QCML: Quadratic Cone Modeling Language
======================================

[![Build Status](https://travis-ci.org/cvxgrp/qcml.png)](https://travis-ci.org/cvxgrp/qcml)

**This repository is currently a work in progress
If you wish to use this in your project, please contact
[us](mailto:echu508@stanford.edu).**

This project is a modular convex optimization framework for solving
*second-order cone* optimization problems (SOCP). It separates the parsing
and canonicalization phase from the code generation and solve phase. This
allows the use of a unified (domain-specific) language in the front end to
target different use cases.

For instance, a simple portfolio optimization problem can be specified as a
Python string as follows:

    """
    dimensions m n

    variable x(n)
    parameter mu(n)
    parameter gamma positive
    parameter F(n,m)
    parameter D(n,n)
    maximize (mu'*x - gamma*(square(norm(F'*x)) + square(norm(D*x))))
        sum(x) == 1
        x >= 0
    """

Our tool parses the problem and rewrites it, after which it can generate
Python code or external source code. The basic workflow is as follows
(assuming `s` stores a problem specification as above).

    p.parse(s)
    p.canonicalize()
    p.dims = {'m': 5}
    p.codegen('python')
    socp_data = p.prob2socp({'mu':mu, 'gamma':1,'F':F,'D':D}, {'n': 10})
    sol = ecos.solve(**socp_data)
    my_vars = p.socp2prob(sol['x'], {'n': 10})

We will walk through each line:

1. `parse` the optimization problem and check that it is convex
2. `canonicalize` the problem by symbolically converting it to a second-order
   cone program
3. assign some `dims` of the problem; others can be left abstract (see [below] (#abstract-dimensions))
4. generate `python` code for converting parameters into SOCP data and for 
   converting the SOCP solution into the problem variables
5. run the `prob2socp` conversion code on an instance of problem data, pulling 
   in local variables such as `mu`, `F`, `D`; because only one dimension was 
   specified in the codegen step (3), the other dimension must be supplied when 
   the conversion code is run
6. call the solver `ecos` with the SOCP data structure
7. recover the original solution with the generated `socp2prob` function; 
   again, the dimension left unspecified at codegen step (3) must be given here

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

You can include the header and source files with any project, although you must
supply your own solver. The code simply stuffs the matrices for you; you are
still responsible for using the proper solver and linking it. An example of how
this might work is in `examples/lasso.py`.

The `qc_utils` files are static; meaning, if you have multiple sources you wish 
to use in a project, you only need one copy of `qc_utils.h` and `qc_utils.c`.

The generated code uses portions of CSparse, which is LGPL. Although QCML is 
BSD, the generated code is LGPL for this reason.

For more information, see the [features](#features) section.

Prerequisites
=============
For the most basic usage, this project requires:

* Python 2.7.2+ (no Python 3 support yet)
* [PLY](http://www.dabeaz.com/ply/), the Python Lex-Yacc parsing framework.  
Available as python-ply or py-ply package in most distributions
* [ECOS](http://github.com/ifa-ethz/ecos)
* [NUMPY](http://numpy.org)
* [SCIPY](http://scipy.org)

For (some) unit testing, we use [Nose](http://nose.readthedocs.org).

Installation
============
Installation should be as easy as

    python setup.py install

After installation, if you have [Nose](http://nose.readthedocs.org) installed,
then typing

    nosetests

should run the simple unit tests. These tests are not exhaustive at the
moment.

A set of examples can be found under the `examples` directory.

Features
========
Basic types
-----------
There are three basic types in QCML:

* `dimension` (or `dimensions` for multiple)
* `parameter` (or `parameters` for multiple)
* `variable` (or `variables` for multiple)

A `parameter` may optionally take a sign (`positive` or `negative`). These are
entirely abstract. All `variables` are currently assumed to be vectors, and
all `parameters` are assumed to be (sparse) matrices. For example, the code

    dimensions m n
    variables x(n) y(m)
    parameter A(m,n) positive
    parameters b c(n)

declares two dimensions, `m` and `n`; two variables of length `n` and `m`,
respectively; an elementwise positive (sparse) parameter matrix, `A`; the
scalar parameter `b`; and the vector parameter `c`.

Abstract dimensions
-------------------
Dimensions are initially specified as abstract values, e.g. `m` and `n` in the 
examples above.  These abstract values must be converted into concrete values
before the problem can be solved.  There are two ways to make dimensions 
concrete: 

1. specified prior to code generation with a call to `dims = {...}`
2. specified after code generation by passing a `dims` dict/struct to the 
   generated functions, e.g. `prob2socp`, `socp2prob`

Any dimensions specified using `dims = {...}` prior to calling `codegen()`
will be hard-coded into the resulting problem formulation functions.  Thus all 
problem data fed into the generated code must match these prespecified 
dimensions.

Alternatively, some dimensions can be left in abstract form for code 
generation.  In this case, problems of variable size can be fed into the 
generated functions, but the dimensions of the input problem must be fed in 
at the same time.  Problem dimensions must also be given at the recovery step 
to allow variables to be recovered from the solver output.

The user may freely mix prespecified and postspecified dimensions.

(A future release may allow some dimensions to be inferred from the size of 
the inputs.)  
<!-- Another possible future change would remove the requirement to 
specify problem dimensions in the variable recovery step by embedding that 
information in the output of the problem formulation function. -->


Parsing and canonicalization
----------------------------
The parser/canonicalizer canoncializes SOCP-representable *convex*
optimization problems into standard form:

    minimize c'*x
    subject to
      G*x + s == h
      A*x == b
      s in Q

where `Q` is a product cone of second-order cones (i.e., `Q = { (t,y) | ||y||
<= t }`), and `x`, `s` are the optimization variables. The
parser/canonicalizer guarantees that all problems will adhere to the
disciplined convex programming (DCP) ruleset and that the problem has the form

    minimize aff
    subject to
      aff == 0
      norm(aff) <= aff

where `aff` is any affine expression. This can also be a maximization or
feasibility problem. If a problem is entered directly in this form, the
parser/canonicalizer will not modify it; in other words, the
parser/canonicalizer is *idempotent* with respect to SOCPs.

Generation and solve
--------------------
The generator/solver can be used in prototyping or deployment mode. In
prototyping mode (or solve mode), a *function* is generated which, when
supplied with the problem parameters, will call an interior-point solver to
solve the problem. In deployment mode (or code generation mode), *source
code* (in a target language) is generated which solves problem instances.  

The generated code can have problem dimensions hard-coded if dims were 
specified prior to codegen, or it can have 
[abstract dimensions] (#abstract-dimensions) to allow problems of variable size 
to be solved.

The valid choice of languages are:

* `"python"` -- emits Python source code
* `"C"` -- emits C source code
* `"matlab"` -- emits Matlab source code
* (planned) `"cvx"` -- emits Matlab source code that calls CVX

With the exception of the `"cvx"` target, the code generator will produce
(at least) two functions in the target language:

* `prob2socp` -- takes problem parameters as input outputs SOCP data, `c`, `G`,
  `h`, `A`, `b`, and the cone descriptions
* `socp2prob` -- takes an SOCP solution and recovers the original variables

If it generates Python code, `exec` is called to create the function
bytecode dynamically, allowing you to call your favorite solver.

If the target "language" is `"cvx"`, we will generate a single Matlab file
that contains the CVX-equivalent problem.

Data structures
---------------
In this section, we document the data structures used for each language.

The input of `prob2socp` is a dictionary/struct containing the parameters.
No sign checking is currently done. Parameters can be scalar, vector, or
(sparse) matrices. In Matlab, these use the native data types. In C, a scalar 
is a `double`, a vector is a `double []`, and a (sparse) matrices is also a
`double []` containing the (nonzero) entries of the matrix in column-manjor 
order. (This corresponds to the storage pattern of column-compressed storage.)
In Python, a scalar is any numeric type, a vector is a Numpy array, and 
(sparse) matrices are Scipy matrices in CSC format.

The output of the `prob2socp` function is a dictionary/struct with the fields:

* `c` -- dense vector
* `G` -- sparse matrix
* `h` -- dense vector
* `dims` -- dict/struct with fields `l` (a number) and `q` (an array)
* `A` -- sparse matrix
* `b` -- dense vector

In Matlab, the vectors and matrices are native. In C, the dense vector is
stored as a C `double` array. The sparse matrices are in column-compressed
format. In Python, vectors are represented by Numpy arrays and sparse matrices
are represented in CSC format.

(Note that the `dims` field in the output of `prob2socp` does not correspond to
the `dims` input into `prob2socp`.  The output represents number of conic 
constraints, while the input specifies input problem dimensions left abstract
at codegen time.  The output might better be named `cones`, but it is called
`dims` to be compatible with the ECOS solver.


Use as embedded language
------------------------
Although QCML's original intent was to be used to parse files with problems
specified in QCML, its Python API has been exposed for use in Python. It
operates similarly to a safe `eval` in Python. Problems can be passed as
strings to the API and prototyping functions can be used to evaluate the
model before asking QCML to generate a solver in a more efficient langauge,
such as in C or CUDA.

Example
=======
As an example, consider the Lasso problem,

    # this entire line is a comment!
    dimensions m n
    variable x(n)
    parameter A(m,n)
    parameter lambda positive

    minimize sum(square(A*x - 4)) + lambda*norm(x)

Note that dimenions are named, but abstract (they do not refer to any
numbers). Similarly, variables and parameters are abstract, their shape is
denoted only by references to named dimensions. Although matrix variable
*declarations* are possible, QCML's behavior is undefined (and may possibly
fail). Matrix variables (along with `for` loops, concatenation, and slicing)
are planned for a future release.
<!--
Some currently available keywords are:

* `dimension`, `dimensions`
* `variable`, `variables`
* `parameter`, `parameters`
* `positive`, `nonnegative`
* `negative`, `nonpositive`
* `minimize`
* `maximize`
-->

QCML canonicalizes this problem to an SOCP.

Inside Python, the code might look like

    from qcml import QCML
    if __name__ == '__main__':
        p = QCML()

        p.parse("""
          # this entire line is a comment!
          dimension n
          dimension m
          variable x(n)
          parameter A(m,n)
          parameter lambda positive

          minimize sum(square(A*x - 4)) + lambda*norm(x)
        """)
        
        p.canonicalize()

This will canonicalize the problem and build an internal problem parse
tree inside Python.  Once the problem has been canonicalized, the user can
decide to either generate a function to prototype problems or generate source
code. For instance, the following lines will create a solver function
`f` and call the solver, with the parameter arguments supplied.

    p.dims = {'m':m, 'n':n}
    p.codegen("python")  # this creates a solver in Python calling CVXOPT
    f = p.solver
    f({'A': A, 'lambda':0.01})

Note that this is not possible with the code generators for other, external
languages.

<!-- Parameter dimensions and sparsity patterns are assumed to be unknown *until* the generated function is run. That is, after the code has been parsed and a function generated, the user will not be able to know that parameters have the wrong dimensions or storage format until attempting to run the function. This is by design.

Once the user feels that the model is sufficient and wishes to scale to a problem with more data, the following line might conceivably generate source code to solve a problem instance (with fixed dimensions and sparsity pattern).

    p.generateC(x = 1e6, A = sparsity_pattern)

The argument corresponding to the variable name gives the length of the vector, and an exemplar for the sparsity pattern of parameters may be provided in lieu of its dimensions.

Note that SCOOP is stateful, so one could use Python as a templating language and declare multiple variables

    for i in range(n):
      p.run("variable x%d scalar" % i) -->


<!-- Scientific computing mode
=========================
Parse tree only produces a list of linear functions. These are repeatedly evaluated in the solver. -->

Operators and atoms
===================
QCML provides a set of linear operators and atoms for use with modeling.
Since an SOCP only consists of affine functions and second-order cone
inequalities, we only provide linear operators and operators for constructing
second-order cones. All other atoms are implemented as *macros*. Whenever the
parser encounters an atom, it simply expands its definition.

Operators
---------
The standard linear operators are:

* infix operators
  * `+`
  * `-`
  * `*`, lhs *must* be a parameter
  * `\`, rhs and lhs *must* be numeric constants
* prefix operators
  * `-`, unary minus / negate
* vector operators (map vectors to scalars)
  * `sum(x)`
  * `sum(x,y,..)`, defined as `x + y + ...`

The operators used for constructing second-order cones are:

* scalar operators (map scalars to scalars)
  * `abs(x)`
* vector operators (map vectors to scalars)
  * `norm(x)`
  * `norm2(x)`, equivalent to `norm(x)`

Atoms
-----
The atoms we provide are:

* scalar atoms (map scalars to scalars)
  * `pos(x)`, defined as `max(x, 0)`
  * `neg(x)`, defined as `max(-x, 0)`
  * `square(x)`
  * `inv_pos(x)`
  * `geo_mean(x,y)`
  * `sqrt(x)`
* vector atoms (map vectors to scalars)
  * `max(x)`, the max elem of `x`
  * `max(x,y,..)`, the max vector consisting of max elements across rows
  * `min(x)`, the min elem of `x`,
  * `min(x,y,..)`, the min vector consisting of min elements across rows
  * `quad_over_lin(x,y)`, if `y` is a vector, returns element-wise operator
  * `norm1(x)`, defined as `sum(abs(x))`
  * `norm1(x,y,..)`, defined as `abs(x) + abs(y) + ...`
  * `norm_inf(x)`, defined as `max(abs(x))`
  * `norm_inf(x,y,...)`, defined as `max(abs(x),abs(y),...)`

Roadmap
=======
In no particular order, the future of this project...

* C code generation for ECOS
* CUDA and GPU support for large-scale solvers
* test cases
* example suite
* user guide
* a solver based on scientific computing (just walks parse trees)

Support
=======
This project is supported in large part by an XDATA grant, supported by the
Air Force Research Laboratory grant FA8750-12-2-0306.


<!-- Syntax
------
The input language follows a syntax similar to CVXGEN. If you are familiar
with imperative languages, then it shouldn't be too unfamiliar. Dimensions,
parameters, and variables (also called symbols) must be declared before use.
You cannot forward-declare these. However, you can declare them in whichever
order as long as you do not refer to undefined symbols: for instance, you
could write

    parameter b
    dimension n = 10
    variable x(n)

As a consequence, the problem declaration must appear last.

### Concatenation ###
Only *vertical* concatenation is allowed. This is accomplished using Matlab
syntax:

    [x; y]

will concatenate two vector expressions vertically. It will not keep track of
individual properties. Instead, if a positive and negative (or convex and
concave) expression are concatenated, the result is a vector of unknown sign
(or curvature).


Sample problem
--------------
As an example, consider the following problem:

    dimension n = 5
    dimension m = 10

    parameter A(m,n)
    parameter b(m)
    parameter lambda positive
    variable x(n)

    minimize square(norm(A*x - b)) + lambda*norm1(x)
    subject to
      x >= 0

Sample output
-------------
Assuming the example text is saved to a file called `example.prob`, running `./efe --ecos example.prob` will produce:

    c_ = sparse(53,1);
    c_(53) = 1;
    b_ = sparse(41,1);
    b_(2:2) = -0.5*ones(1, 1);
    b_(3:3) = -0.5*ones(1, 1);
    b_(24:33) = b;
    A_ = sparse(41, 53);
    A_(1:1, 29:29) = 1*ones(1, 1); A_(1:1, 30:30) = 1*speye(1, 1); A_(1:1, 53:53) = -1*speye(1, 1);
    A_(2:2, 29:29) = 0.5*speye(1, 1); A_(2:2, 1:1) = -1*speye(1, 1);
    A_(3:3, 29:29) = -0.5*speye(1, 1); A_(3:3, 2:2) = -1*speye(1, 1);
    A_(4:13, 31:40) = 1*speye(10, 10); A_(4:13, 41:50) = -1*speye(10, 10); A_(4:13, 4:13) = -1*speye(10, 10);
    A_(14:23, 19:23) = A; A_(14:23, 31:40) = -1*speye(10, 10);
    A_(24:33, 41:50) = 1.0*speye(10, 10);
    A_(34:34, 51:51) = lambda*speye(1, 1); A_(34:34, 30:30) = -1*speye(1, 1);
    A_(35:35, 14:18) = 1*ones(1, 5); A_(35:35, 51:51) = -1*ones(1, 1);
    A_(36:40, 19:23) = 1*speye(5, 5); A_(36:40, 24:28) = -1*speye(5, 5); A_(36:40, 52:52) = -1*ones(5, 1);
    A_(41:41, 52:52) = 1.0*speye(1, 1);
    G_ = sparse(29, 53);
    G_(1:1:5, 24:28) = -speye(5, 5);
    G_(6:2:15, 14:18) = -speye(5, 5);
    G_(7:2:16, 19:23) = -speye(5, 5);
    G_(16:1:16, 1:1) = -speye(1, 1);
    G_(17:1:17, 2:2) = -speye(1, 1);
    G_(18:1:18, 3:3) = -speye(1, 1);
    G_(19:1:19, 3:3) = -speye(1, 1);
    G_(20:1:29, 4:13) = -speye(10, 10);
    h_ = zeros(29, 1);
    dims.q = [2,2,2,2,2,3,11];
    dims.l = 5;

    [x_codegen, y_, info_] = ecos(full(c_), G_, h_, dims, A_, full(b_));

    t1z0 = x_codegen(1:1);
    t1z1 = x_codegen(2:2);
    t2 = x_codegen(3:3);
    t3 = x_codegen(4:13);
    t7z0 = x_codegen(14:18);
    x = x_codegen(19:23);
    xGTt8 = x_codegen(24:28);
    t1 = x_codegen(29:29);
    t6 = x_codegen(30:30);
    t4 = x_codegen(31:40);
    t5 = x_codegen(41:50);
    t7 = x_codegen(51:51);
    t8 = x_codegen(52:52);
    t0 = x_codegen(53:53);
    ecos_optval = 1*info_.pcost;

This file can be run inside Matlab and assumes that the parameters named `A`,
`b`, and `lambda` exist in the namespace. It doesn't check if `lambda` is
actually positive. -->
