"""
Tests that function prototypes are properly generated. Does not check that
the function body is properly generated.

Should be able to run generated Makefile without any errors (except warnings).
"""
import os, shutil, subprocess

from .. codegens import PythonCodegen, MatlabCodegen, C_Codegen
from nose import with_setup

LP = """
variable x
parameter c
minimize c*x
x >= 0
"""
SOCP = """
variable x
parameter c
minimize c*x
norm(x) <= 0
"""

C = C_Codegen(name="test_problem")
python = PythonCodegen()
matlab = MatlabCodegen()

codegens = [C, python, matlab]

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
    assert python.prob2socp.source == "def prob_to_socp(params, dims={}):\n    pass"

def test_empty_python_socp2prob():
    assert python.socp2prob.source == "def socp_to_prob(x, dims={}):\n    pass"

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

def compiles(with_cplus_plus=False):
    try:
        with open(os.devnull, "w") as fnull:
            if with_cplus_plus:
                subprocess.check_call(["make", "-C", "test_problem", "CC=c++"], stdout=fnull, stderr=fnull)
            else:
                subprocess.check_call(["make", "-C", "test_problem"], stdout=fnull, stderr=fnull)
    except subprocess.CalledProcessError as e:
        # compilation fails
        print "Generated (empty) C code unable to compile."
        print C.prob2socp.source
        print
        print C.socp2prob.source
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

    yield compiles, False
    yield compiles, True

def parse_and_generate(prob, lang):
    from .. qc_lang import QCML
    p = QCML(debug=True)
    p.parse(prob)
    p.canonicalize()
    if lang == "C":
        p.codegen(lang, name="test_problem")
        shutil.rmtree("%s/test_problem" % os.getcwd())
    else:
        p.codegen(lang)
    
    # only checks for exceptions
    assert True

def test_parse_and_compiles():
    yield parse_and_generate, LP, "python"
    yield parse_and_generate, LP, "matlab"
    yield parse_and_generate, LP, "C"
    
    yield parse_and_generate, SOCP, "python"
    yield parse_and_generate, SOCP, "matlab"
    yield parse_and_generate, SOCP, "C"
    

