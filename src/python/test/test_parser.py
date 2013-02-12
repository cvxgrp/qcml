from nose.tools import assert_raises, with_setup
from scoop import Scoop
from scoop.expression import isscalar, isvector, ismatrix, Sign, AFFINE
from collections import deque

p1 = Scoop()
p2 = Scoop()
# these aren't exposed in scoop, so they are redefined here
PRE_OBJ, OBJ, POST_OBJ = range(3)

check = {
    'SCALAR': isscalar,
    'VECTOR': isvector,
    'MATRIX': ismatrix
}

def setup_func():
    "set up test fixtures"
    p1.run("variable x")  # mangles name
    p2.run("variable y")

def teardown_func():
    "tear down test fixtures"
    p1.clear()
    p2.clear()


# assert empty name throw exception
@with_setup(setup_func, teardown_func)
def test_empty_names():
    yield assert_raises, Exception, p1.parse_variable, deque([("VARIABLE","")])
    yield assert_raises, Exception, p2.parse_parameter, deque([("PARAMETER","")])
    
# assert that the name already exists
@with_setup(setup_func, teardown_func)
def test_existing_names():
    yield assert_raises, Exception, p1.parse_variable, deque([("VARIABLE",""),("IDENTIFIER", "_x")])
    yield assert_raises, Exception, p2.parse_parameter, deque([("PARAMETER",""),("IDENTIFIER", "_y")])

# add a variable
@with_setup(setup_func, teardown_func)
def add_variable(s):
    p1.parse_variable(deque([("VARIABLE",""),("IDENTIFIER", "x"), (str.upper(s), s)]))
    assert(check[str.upper(s)](p1.symtable["x"].shape))
    assert(p1.symtable["x"].sign == Sign("UNKNOWN"))
    assert(p1.symtable["x"].vexity == AFFINE)

# add a vector parameter
@with_setup(setup_func, teardown_func)
def add_parameter(s1, s2, shape_first = True):
    p1.parse_parameter(deque([("PARAMETER",""),("IDENTIFIER", "x"), (str.upper(s1), s1), (str.upper(s2), s2)]))
    if shape_first:
        assert(check[str.upper(s1)](p1.symtable["x"].shape))
        assert(p1.symtable["x"].sign == Sign(str.upper(s2)))
        assert(p1.symtable["x"].vexity == AFFINE)
    else:
        assert(check[str.upper(s2)](p1.symtable["x"].shape))
        assert(p1.symtable["x"].sign == Sign(str.upper(s1)))
        assert(p1.symtable["x"].vexity == AFFINE)
    

shapes = ["SCALAR", "VECTOR", "MATRIX"]
signs = ["positive", "negative"]
# assert that shape and sign can be specified in any way
def test_shape_and_sign():
    # signed variable
    yield assert_raises, Exception, p1.parse_variable, deque([("VARIABLE",""),("IDENTIFIER", "_x"), ("POSITIVE", "positive")])
    # matrix variable
    yield assert_raises, Exception, p1.parse_variable, deque([("VARIABLE",""),("IDENTIFIER", "_x"), ("MATRIX", "matrix")])
    # too many args
    yield assert_raises, Exception, p1.parse_variable, deque([("VARIABLE",""),("IDENTIFIER", "_x"), ("SCALAR", "matrix"), ("POSITIVE", "positive")])
    
    # test addition of variables
    yield add_variable, "VECTOR"
    yield add_variable, "SCALAR"
    
    # test addition of various parameters
    for shape in shapes:
        for sign in signs:
            yield add_parameter, shape, sign, True
            yield add_parameter, sign, shape, False
        yield assert_raises, SyntaxError, add_parameter, shape, "UNKNOWN", True
        yield assert_raises, SyntaxError, add_parameter, "UNKNOWN", shape, False

# simple way to create names
def add_name(p, name):
    p.parse_variable(deque([("VARIABLE",""), ("IDENTIFIER","%s" % name)]))
    assert(name in p.symtable)
    
# assert that we can create new names
@with_setup(setup_func, teardown_func)
def test_new_names():
    yield add_name, p1, "_y"
    yield add_name, p2, "_x"
   
@with_setup(setup_func, teardown_func) 
def test_clear():
    assert(len(p1.symtable) > 0)
    assert(p1.line is not "")
    p1.clear()
    assert(not p1.symtable)
    assert(not p1.line)
    assert(p1.varcount == 0)
    assert(p1.state == PRE_OBJ)
    

# TODO: bad operations
# TODO: bad expressions