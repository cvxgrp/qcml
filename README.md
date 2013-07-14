Quadratic Cone Modeling Language (QCML)
=======================================

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
Python code of external source code. The basic workflow is as follows
(assuming `s` stores a problem specification).

    p.parse(s)
    p.canonicalize()
    p.set_dims({'m': m, 'n': n})
    p.codegen("cvxopt")
    solution = p.solver({'gamma':1,'F':F,'D':D})

The third line sets the dimensions of the problem and the last line calls
the (Python) solver generated in the codegen step. This
solver requires that the user specify the parameters in their
optimization model. For rapid prototyping, we provide the convenience
function:

    solution = p.solve(locals())

This functions wraps the last four steps above into a single call and
assumes that all parameters and dimensions are defined in the local
namespace.

Prerequisites
=============
For the most basic usage, this project requires:

* Python 2.7.2+ (no Python 3 support yet)
* [CVXOPT](http://abel.ee.ucla.edu/cvxopt/)

For (some) unit testing, we use [Nose](http://nose.readthedocs.org).

Depending on the type of code you generate, you may also need:

* Matlab
* [CVX](http://cvxr.com)
* [PDOS](http://github.com/cvxgrp/pdos)
* [ECOS](http://github.com/ifa-ethz/ecos)

PDOS is an experimental first-order solver, and we recommend CVXOPT or ECOS
over it.

Installation
============
Installation should be as easy as

    cd src
    python setup.py install

After installation, if you have [Nose](http://nose.readthedocs.org) installed,
then typing

    nosetests scoop

should run the simple unit tests. These tests are not exhaustive at the
moment.

The only working sample scripts are `qcml_example.py` and `main.py`.
The others refer to an older implementation. The `main.py` script takes
command line arguments and emits Matlab code used to call ECOS.


Features
========

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
code* (in a target language) is generated which solves a particular problem
instance with fixed dimensions.

In prototyping mode, the problem data may change with each invocation of
the generated function. If problem dimensions change, you must set the
dimensions of the QCML object and codegen the Python function again.
In deployment mode, the problem dimensions are fixed,
but problem data is allowed to change.

The valid choice of solvers are:

* `"cvx"` -- emits Matlab source code that calls CVX
* `"cvxopt"` -- emits Python source code that calls CVXOPT
* `"ecos"` -- emits Python source code that calls ECOS
* `"matlab"` -- emits Matlab source code that calls ECOS
* `"PDOS"` -- emits Python source code that calls PDOS

When these solvers are supplied as arguments to the code generator,
it produces code of the appropriate language (Python or Matlab).
If it generates Python code, `exec` is called to create the function
bytecode dynamically, allowing you to call the solver.

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
numbrs). Similarly, variables and parameters are abstract, their shape is
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

    from scoop import QCML
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
        p.set_dims({'m':m, 'n':n})
        p.prettyprint()

Thsis will canonicalize the problem and build an internal problem parse
tree inside Python. Once the problem has been canonicalized, the user can
decide to either generate a function to prototype problems or generate source
code. For instance, the following three lines will create a solver function
`f` and call the solver, with the parameter arguments supplied.

    p.codegen("cvxopt")  # this creates a solver in Python calling CVXOPT
    f = p.solver
    f({'A': A, 'lambda':0.01})

Note that this is not possible with one of the Matlab code generators.

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

### Vector transpose ###
At the moment, we don't support vector transposes. For linear objectives such
as `c'*x`, the parameter has to be transposed externally and stated as
`parameter ct(1,n)` instead of `parameter c(n)`.

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
