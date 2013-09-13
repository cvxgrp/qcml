from qcml import QCML
from qcml.errors import QC_ParseError, QC_DCPError
from nose.tools import assert_raises
import os

""" Checks that the problems in the test/problems directory are either
    properly parsed or throw the expected error. Also checks some simple
    problems in this file.
"""

def get_filename(x):
    return "%s/problems/test%s.prob" % (os.path.dirname(__file__),x)

good_problems = map(get_filename, [1,2,5,6])
nondcp_problems = map(get_filename, [3,4])
syntax_error_problems = map(get_filename, [7,8])

def parse(s):
    p = QCML(debug=True)
    p.parse(s)
    assert True # if we get here without error, we succeeded

def check_problem(file):
    with open(file, "r") as f:
        prob = f.read()
    parse(prob)

keyword_list = [
    "variable x\n",
    "dimension m",
    "parameter A positive",
    "variables x y z",
    "parameters A b c",
    "dimensions m n f"
]

problem_list = [
"""
    dimensions length_of_car
    variable x(length_of_car)
    parameter A(length_of_car,length_of_car) nonnegative
""",
"""
    dimensions n
    variable foolishness(n)
    maximize sum(foolishness)
""",
""" dimensions n
    variable x(n)
    parameter lambda positive
    # variable y
    minimize lambda*sum(x) + sum(square(x))
    subject to

""",
""" dimensions n
    variable x(n)
    parameter lambda positive
    x >= 0
    minimize lambda*sum(x) + sum(square(x))
    subject to
        x <= 4
        # bogus stuff
""",
""" dimension n
    variable x(n)
    parameter c(n)
    minimize c'*x
"""
]

bad_problem_list = [
"dimension n m",
"dimensions n, m",
"variable x y",
"parameter a b",
"parameter a b positive",
"parameters a b positive",
"dimension n nonnegative",
"variable x(n)",
"variable x positive",
""" dimension n
    variable x(n)
    parameter b positive

    minimize sum(b*x)+""",
]

# check keywords: variable, parameters, and dimensions
def test_keywords():
    # ensure that these are valid statements
    for keyword in keyword_list:
        yield parse, keyword

def test_problem():
    # ensure that these are valid problems
    for problem in problem_list:
        yield parse, problem

    for p in good_problems:
        yield check_problem, p

def test_bad_problem():
    # ensure that these fail
    for problem in bad_problem_list:
        yield assert_raises, QC_ParseError, parse, problem

    for p in nondcp_problems:
        yield assert_raises, QC_DCPError, check_problem, p

    for p in syntax_error_problems:
        yield assert_raises, QC_ParseError, check_problem, p

def test_unused_variable():
    p = QCML(debug=True)
    p.parse("""
        dimension n
        variables x(n) y
        minimize sum(x)
    """)
    print p.problem
    print p.problem.variables
    assert p.problem.variables.keys() == ['x']

def test_unused_parameter():
    p = QCML(debug=True)
    p.parse("""
        dimension n
        parameters x(n) y
        minimize sum(x)
    """)
    print p.problem
    print p.problem.parameters
    assert p.problem.parameters.keys() == ['x']

def test_unused_dimensions():
    p = QCML(debug=True)
    p.parse("""
        dimensions m n
        variables x(n) y
        minimize sum(x)
    """)
    print p.problem
    print p.problem.dimensions
    assert p.problem.dimensions == set(['n'])

    #yield assert_raises, Exception, p1.parse_variable, deque([("VARIABLE","")])

# from nose.tools import assert_raises, with_setup
# from scoop import Scoop
# from scoop.expression import isscalar, isvector, ismatrix, Sign, AFFINE
# from collections import deque
#
# p1 = Scoop()
# p2 = Scoop()
# # these aren't exposed in scoop, so they are redefined here
# PRE_OBJ, OBJ, POST_OBJ = range(3)
#
# check = {
#     'SCALAR': isscalar,
#     'VECTOR': isvector,
#     'MATRIX': ismatrix
# }
#
# def setup_func():
#     "set up test fixtures"
#     p1.run("variable x")  # mangles name
#     p2.run("variable y")
#
# def teardown_func():
#     "tear down test fixtures"
#     p1.clear()
#     p2.clear()
#
#
# # assert empty name throw exception
# @with_setup(setup_func, teardown_func)
# def test_empty_names():
#     yield assert_raises, Exception, p1.parse_variable, deque([("VARIABLE","")])
#     yield assert_raises, Exception, p2.parse_parameter, deque([("PARAMETER","")])
#
# # assert that the name already exists
# @with_setup(setup_func, teardown_func)
# def test_existing_names():
#     yield assert_raises, Exception, p1.parse_variable, deque([("VARIABLE",""),("IDENTIFIER", "_x")])
#     yield assert_raises, Exception, p2.parse_parameter, deque([("PARAMETER",""),("IDENTIFIER", "_y")])
#
# # add a variable
# @with_setup(setup_func, teardown_func)
# def add_variable(s):
#     p1.parse_variable(deque([("VARIABLE",""),("IDENTIFIER", "x"), (str.upper(s), s)]))
#     assert(check[str.upper(s)](p1.symtable["x"].shape))
#     assert(p1.symtable["x"].sign == Sign("UNKNOWN"))
#     assert(p1.symtable["x"].vexity == AFFINE)
#
# # add a vector parameter
# @with_setup(setup_func, teardown_func)
# def add_parameter(s1, s2, shape_first = True):
#     p1.parse_parameter(deque([("PARAMETER",""),("IDENTIFIER", "x"), (str.upper(s1), s1), (str.upper(s2), s2)]))
#     if shape_first:
#         assert(check[str.upper(s1)](p1.symtable["x"].shape))
#         assert(p1.symtable["x"].sign == Sign(str.upper(s2)))
#         assert(p1.symtable["x"].vexity == AFFINE)
#     else:
#         assert(check[str.upper(s2)](p1.symtable["x"].shape))
#         assert(p1.symtable["x"].sign == Sign(str.upper(s1)))
#         assert(p1.symtable["x"].vexity == AFFINE)
#
#
# shapes = ["SCALAR", "VECTOR", "MATRIX"]
# signs = ["positive", "negative"]
# # assert that shape and sign can be specified in any way
# def test_shape_and_sign():
#     # signed variable
#     yield assert_raises, Exception, p1.parse_variable, deque([("VARIABLE",""),("IDENTIFIER", "_x"), ("POSITIVE", "positive")])
#     # matrix variable
#     yield assert_raises, Exception, p1.parse_variable, deque([("VARIABLE",""),("IDENTIFIER", "_x"), ("MATRIX", "matrix")])
#     # too many args
#     yield assert_raises, Exception, p1.parse_variable, deque([("VARIABLE",""),("IDENTIFIER", "_x"), ("SCALAR", "matrix"), ("POSITIVE", "positive")])
#
#     # test addition of variables
#     yield add_variable, "VECTOR"
#     yield add_variable, "SCALAR"
#
#     # test addition of various parameters
#     for shape in shapes:
#         for sign in signs:
#             yield add_parameter, shape, sign, True
#             yield add_parameter, sign, shape, False
#         yield assert_raises, SyntaxError, add_parameter, shape, "UNKNOWN", True
#         yield assert_raises, SyntaxError, add_parameter, "UNKNOWN", shape, False
#
# # simple way to create names
# def add_name(p, name):
#     p.parse_variable(deque([("VARIABLE",""), ("IDENTIFIER","%s" % name)]))
#     assert(name in p.symtable)
#
# # assert that we can create new names
# @with_setup(setup_func, teardown_func)
# def test_new_names():
#     yield add_name, p1, "_y"
#     yield add_name, p2, "_x"
#
# @with_setup(setup_func, teardown_func)
# def test_clear():
#     assert(len(p1.symtable) > 0)
#     assert(p1.line is not "")
#     p1.clear()
#     assert(not p1.symtable)
#     assert(not p1.line)
#     assert(p1.varcount == 0)
#     assert(p1.state == PRE_OBJ)
#
#
# # TODO: bad operations
# # TODO: bad expressions
