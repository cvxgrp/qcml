""" Verifies that the problem tree is properly shown.
"""
from nose.tools import assert_raises
from .. errors import QC_DCPError
from .. ast import Objective, Problem
from .. ast.atoms import atom

from .. ast.expressions import expression as e
from .. properties import shape
import StringIO

x = e.Variable('x', shape.Vector('n'))
normx = atom.QC_norm(x)
prob = Problem(Objective("minimize", normx), [x == e.Number(1), e.Number(1) >= x, x <= e.Number(1)])


exp_result = """%sDCP objective: %s norm(x)
  QC_norm: convex, positive, Scalar()
    Variable: affine, neither, Vector(n), x
"""

parsed_prob = """DCP program:
  DCP objective: minimize norm(x)
    QC_norm: convex, positive, Scalar()
      Variable: affine, neither, Vector(n), x
  LinearEquality: True, Vector(n)
    Add: affine, neither, Vector(n)
      Variable: affine, neither, Vector(n), x
      Mul: constant, negative, Scalar()
        Number: constant, negative, Scalar(), -1
        Number: constant, positive, Scalar(), 1
  LinearInequality: True, Vector(n)
    Add: affine, neither, Vector(n)
      Variable: affine, neither, Vector(n), x
      Mul: constant, negative, Scalar()
        Number: constant, negative, Scalar(), -1
        Number: constant, positive, Scalar(), 1
"""

canon_prob = """DCP program:
  DCP objective: minimize _t0
    Variable: affine, neither, Scalar(), _t0
  LinearEquality: True, Vector(n)
    Add: affine, neither, Vector(n)
      Number: constant, negative, Scalar(), -1
      Variable: affine, neither, Vector(n), x
  LinearInequality: True, Vector(n)
    Add: affine, neither, Vector(n)
      Number: constant, negative, Scalar(), -1
      Variable: affine, neither, Vector(n), x
  SOC: True, Scalar()
    Variable: affine, neither, Scalar(), _t0
    Variable: affine, neither, Vector(n), x
"""

def check(obj, exp):
    output = StringIO.StringIO()
    obj.show(output)
    contents = output.getvalue()
    print (exp)
    print (contents)
    assert exp == contents

def check_constr(prob, exp):
    for c,e in zip(prob.children(), exp):
        print c
        print e
        assert(c is e)

def test_unknown_sense_error():
    yield assert_raises, Exception, Objective, "hello", x

def test_dcp_error():
    yield assert_raises, QC_DCPError, Objective, "find", x

def test_objective():
    yield check, Objective("minimize", normx), exp_result % ("", "minimize")
    yield check, Objective("maximize", normx), exp_result % ("Non-", "maximize")

def test_add_constraint():
    o = Objective("maximize", normx)
    local_prob = Problem(o, [])
    yield check_constr, local_prob, [o]

    # checks that if you add multiple constraints, removes duplicates
    p1 = x == e.Number(1)
    p2 = x <= e.Number(2)
    p3 = e.Number(2) >= x
    local_prob.add_constraint(p1)
    local_prob.add_constraint(p2)
    local_prob.add_constraint(p3)

    yield check_constr, local_prob, [o,p1,p2]

def test_program():
    yield check, prob, parsed_prob
    prob.canonicalize()
    yield check, prob, canon_prob
