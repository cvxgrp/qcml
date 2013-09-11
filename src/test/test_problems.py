from qcml import QCML, QCError
from nose.tools import assert_raises
import os

""" Checks that the problems in the test/problems directory are either
    properly parsed and canonicalized or throw the expected error.
"""

def get_filename(x):
    return "%s/problems/test%s.prob" % (os.path.dirname(__file__),x)

good_problems = map(get_filename, [1,2,5,6])
nondcp_problems = map(get_filename, [3,4])
syntax_error_problems = map(get_filename, [7,8])

def check_problem(file):
    with open(file, "r") as f:
        prob = f.read()

    p = QCML(debug=True)
    p.parse(prob)        
    p.canonicalize()

def test_parse_and_canonicalize():
    for p in good_problems:
        yield check_problem, p
    
    for p in nondcp_problems:
        yield assert_raises, Exception, check_problem, p
    
    for p in syntax_error_problems:
        yield assert_raises, QCError, check_problem, p

