ECOS front end (EFE)
====================

This project contains a front end for the *embedded conic solver* (`ECOS`) to
solve *second-order cone problems* (SOCP). In `ECOS`, a second-order cone
problem is

    minimize c'*x
    subject to
      G*x + s == h
      A*x == b
      s In K

where `K` is a product cone of linear and second-order cones..

The front end provides an input language (and accompanying syntax) used to
enter problems in text files. The front end is called with the text file
supplied as argument to "compile" the code in to four different targets:

* `CVX` -- This converts the problem in to an SOCP (with new variables) and
  outputs a CVX problem with only equality constraints and cone constraints.
  The equality constraints are given literally in terms of the variable names
  (i.e., they are not stuffed in to matrices).

* `CVX` cone solver -- This converts the equality constraints in to matrices
  and bundles all the new variables in to a single vector optimization
  variable. The CVX problem solved is
        
        minimize c'*x
        subject to
          A*x == b
          x In K
          
  where the cone `K` is entered literally (i.e., `norm([x(2),x(3)]) <= x(1)`).

* `conelp` -- This performs the needed matrix stuffing and outputs the data
  `A`, `b`, `G`, and `h` and calls the `conelp` solver in Matlab.

* `ECOS` -- This uses the same matrix stuffing and produces a call to
  the `ECOS` solver in Matlab.

Directory structure
-------------------
This project is divided in to the following folders:

* `src` -- Haskell source for the code generator
* `doc` -- Latex documentation for the code generator
* `matlab` -- Matlab test functions

Dependencies
------------
This project requires:

* Haskell -- The front end is written in [Haskell](http://www.haskell.org) and
  requires a Haskell installation to compile. We've tested it with the GHC 
  compiler. You're on your own with a different Haskell implementation.
  * Parsec -- This is a Haskell library that powers the parser. It should come 
    installed with Haskell/GHC.
  * HUnit -- This is a unit testing library that we (minimally) use. It should 
    also come installed with Haskell/GHC.
* `ECOS` -- The generated code calls the `ECOS` solver. Although the
  code generator can be used to produce calls to `CVX`, this is less
  interesting.
* Matlab -- Matlab is used for testing the generated problems.

Currently, Matlab is a required dependency since the code generator does not
actually emit source code; instead, the code generator will produce lines of
Matlab code and calls the `ECOS` solver. This is for testing purposes mostly
while we iron out the implementation.

Optional dependencies are:

* `CVX` -- Calls to `CVX` are made to verify results. More information on
  `CVX` can be found [here](http://cvxr.com).

Installation
------------
To install the ECOS front end, ensure that Haskell is installed and run the
following lines of code

    cd src
    make

This will produce three binaries: `main`, `test`, and `efe`. At the
moment, only `efe` does anything. The other two binaries are in various
states of neglect. The `efe` binary takes a text file as input and
outputs the CVX representation of the problem (if the problem is DCP
compliant).


EFE
---
The `efe` binary takes as input a path to a text file that specifies an
optimization problem using the front end language. The target output can be
toggled with a command line option

* `cvx` -- cvx output
* `cvxsocp` -- cvx socp output
* `conelp` -- conelp matlab output
* `ecos` -- ecos / paris matlab output.

Running `efe` without any arguments will produce a list of options. It writes
the generated code to `stdout`.

ECOS tester
-----------
Under the `matlab` folder, a test driver called `ecos_tester` runs a suite of
problem tests. It compares the results to expected `CVX` results.

Some tests may fail because `ECOS` is not a high precision solver.

Available atoms
---------------
* infix atoms
  * `+`
  * `-`
  * `*`, rhs *must* be a parameter
* prefix atoms
  * `-`, unary minus / negate
* scalar atoms (map scalars to scalars)
  * `pos(x)`, defined as `max([x; 0])`
  * `neg(x)`, defined as `max([-x; 0])`
  * `square(x)`
  * `inv_pos(x)`
  * `abs(x)`
  * `geo_mean(x,y)`
  * `sqrt(x)`
* vector atoms (map vectors to scalars)
  * `sum(x)`
  * `max(x)`
  * `min(x)`
  * `quad_over_lin(x,y)`, second argument must be scalar
  * `norm(x)`
  * `norm2(x)`, equivalent to `norm(x)`
  * `norm1(x)`
  * `norm_inf(x)`

Syntax
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
actually positive.