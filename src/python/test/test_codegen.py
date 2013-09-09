"""
Tests that function prototypes are properly generated. Does not check that
the function body is properly generated.

Should be able to run generated Makefile without any errors (except warnings).
"""
import os, shutil, subprocess

from qcml.codegens import PythonCodegen, MatlabCodegen, C_Codegen
from nose import with_setup

#TODO: matlab = MatlabCodegen()
C = C_Codegen({}, name="test_problem")
python = PythonCodegen({})

codegens = [C, python]

def codegen(codegen_obj):
    # test that empty code generates executes without error
    try:
        codegen_obj.codegen()
    except:
        assert False
    else:
        assert True

def test_codegen():
    for gen in codegens:
        yield codegen, gen

def test_empty_python_prob2socp():
    assert python.prob2socp.source == "def prob_to_socp(params):\n    pass"

def test_empty_python_socp2prob():
    assert python.socp2prob.source == "def socp_to_prob(x):\n    pass"

c_files = [
    "test_problem",
    "test_problem/Makefile",
    "test_problem/test_problem.c",
    "test_problem/test_problem.h",
    "test_problem/qcml_utils.c",
    "test_problem/qcml_utils.h"
]

def exists(filename):
    assert os.path.exists("%s/%s" % (os.getcwd(), filename))

def compiles():
    try:
        with open(os.devnull, "w") as fnull:
            subprocess.check_call(["make", "-C", "test_problem"], stdout=fnull, stderr=fnull)
    except subprocess.CalledProcessError:
        # compilation fails
        print "Generated (empty) C code unable to compile."
        assert False
    except OSError:
        # make is not installed
        # skip the compilation test with make
        print "Could not find 'make' in the system. Not able to check if code compiles."
        assert False
    except:
        raise
    else:
        assert True


def setup_func():
    pass

def teardown_func():
    # remove the generated directory
    shutil.rmtree("%s/test_problem" % os.getcwd())

@with_setup(setup_func, teardown_func)
def test_C():
    for f in c_files:
        yield exists, f
    
    yield compiles
    

