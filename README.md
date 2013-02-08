Second-order Cone Optimization Parser (SCOOP)
=============================================

This project is a modular convex optimization framework for solving
*second-order cone* optimization problems (SOCP). It separates the parsing
and canonicalization phase from the generation and solve phase. This allows
the use of a unified (domain-specific) language in the front end to target
different use cases.

(XXX: a term rewriting / macro expansion language)

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

In prototyping mode, the dimensions and the problem data may change with each invocation of the generated function. In deployment mode, problem dimensions are fixed, but problem data is allowed to change.

Use as embedded language
------------------------
Although SCOOP's original intent was to be used to parse files with problems specified in SCOOP's language, its Python API has been exposed for use in Python. It operates similarly to a safe `eval` in Python. Problems can be passed as strings to the API and prototyping functions can be used to evaluate the model before asking SCOOP to generate a solver in a more efficient langauge, such as in C or CUDA.

Example
=======
As an example, consider the Lasso problem,

    # this entire line is a comment!
    variable x vector
    parameter A matrix
    parameter lambda scalar positive
    
    minimize sum(square(A*x - 4)) + lambda*norm(x)
    
Note that variables and parameters are abstract, their shape is denoted only
by a shape (`scalar`, `vector`, or `matrix`). SCOOP canonicalizes this
problem to an SOCP.

Inside Python, the code might look like

    from scoop import Scoop
    if __name__ == '__main__':
        p = Scoop()
  
        map (p.run, 
        ["# this entire line is a comment!",
         "variable x vector",
         "parameter A matrix",
         "parameter lambda scalar positive",
         ""
         "minimize sum(square(A*x - 4)) + lambda*norm(x)"])
    
        print p

Thsis will canonicalize the problem and build an internal problem parse tree inside Python. Once the problem has been canonicalized, the user can decide to either generate a function to prototype problems or generate source code. For instance, the following two lines will create a solver function `f` and call the solver, with the parameter arguments supplied.
        
    f = p.generate()
    f(A = spmatrix, lambda = 0.01)

Parameter dimensions and sparsity patterns are assumed to be unknown *until* the generated function is run. That is, after the code has been parsed and a function generated, the user will not be able to know that parameters have the wrong dimensions or storage format until attempting to run the function. This is by design.

Once the user feels that the model is sufficient and wishes to scale to a problem with more data, the following line might conceivably generate source code to solve a problem instance (with fixed dimensions and sparsity pattern).

    p.generateC(x = 1e6, A = sparsity_pattern)

The argument corresponding to the variable name gives the length of the vector, and an exemplar for the sparsity pattern of parameters may be provided in lieu of its dimensions.

Note that SCOOP is stateful, so one could use Python as a templating language and declare multiple variables

    for i in range(n):
      p.run("variable x%d scalar" % i)

Prerequisites
=============
This project requires:

* Python 2.7.2+ (no Python 3 support yet)
* [CVXOPT](http://abel.ee.ucla.edu/cvxopt/)

Depending on the type of code you generate, you may also need:

* Matlab
* [CVX](http://cvxr.com)

<!-- * `ECOS`
* `gcc` (or similar compiler)
* CUDA -->

<!-- Scientific computing mode
=========================
Parse tree only produces a list of linear functions. These are repeatedly evaluated in the solver. -->

Operators and atoms
===================
SCOOP provides a set of linear operators and atoms for use with modeling. Since an SOCP only consists of affine functions and second-order cone inequalities, we only provide linear operators and operators for constructing second-order cones. All other atoms are implemented as *macros*. Whenever the parser encounters an atom, it simply expands its definition.

Operators
---------
The standard linear operators are:

* infix operators
  * `+`
  * `-`
  * `*`, lhs *must* be a parameter
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

* add `pow_rat` to atoms
* parse linear expressions
* fix lookup for atoms defined as composed atoms
* CUDA and GPU support for large-scale solvers
* C code generation
* CVX code generation for verificaton
* test cases
* example suite
* user guide
* a solver based on scientific computing (just walks parse trees)
* hook up to CVXOPT
* a lex/yacc C API?





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