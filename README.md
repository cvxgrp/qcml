ECOS front end
==============

This project contains a front end for the *embedded conic solver* (`ECOS`) for 
*second-order cone problems* (SOCP). In `ECOS`, a second-order cone problem is

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

* `paris`/`ECOS` -- This uses the same matrix stuffing and produces a call to
  the `paris` or `ECOS` solver in Matlab.

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
* `paris`/`ECOS` -- The generated code calls the `ECOS` solver. Although the
  code generator can be used to produce calls to `CVX`, this is less
  interesting.

Optional dependencies are:

* Matlab -- Matlab is used for testing the generated problems.
* `CVX` -- Calls to `CVX` are made to verify results. More information on
  `CVX` can be found [here](http://cvxr.com).

Installation
------------
To install the ECOS front end, ensure that Haskell is installed and run the
following lines of code

    cd src
    make

This will produce three binaries: `main`, `test`, and `probtocvx`. At the
moment, only `probtocvx` does anything. The other two binaries are in various
states of neglect. The `probtocvx` binary takes a text file as input and
outputs the CVX representation of the problem (if the problem is signed DCP
compliant).


ProbToCVX
---------
The `probtocvx` binary takes as input a path to a text file that specifies an
optimization problem using the front end language. Currently, it does not have
the option to change target output. This is hard-coded in to the
`src/Expression/SOCP.hs` file. (It's on my todo list to fix this.)

It writes the generated code to `stdout`.

ECOS tester
-----------
Under the `matlab` folder, a test driver called `ecos_tester` runs a suite of
problem tests. It compares the results to expected `CVX` results.