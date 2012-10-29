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
  requires a Haskell installation to compile.
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