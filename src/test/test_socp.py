""" Verifies that the problem tree is properly shown.

    Also tests the objective and constraints.
"""
from nose.tools import assert_raises
from .. errors import QC_DCPError
from .. import ast
from .. ast.atoms import atom

from .. ast.expressions import expression as e
from .. properties import shape
import StringIO

x = e.Variable('x', shape.Vector('n'))
normx = atom.QC_norm(x)
prob = ast.SOCP(
    ast.ProgramObjective("minimize", normx),
    ast.ProgramConstraints([x == e.Number(1), e.Number(1) >= x, x <= e.Number(1)]),
    ast.ProgramData()
)


exp_result = """%sDCP objective: %s norm(x)
  QC_norm: convex, positive, Scalar()
    Variable: affine, neither, Vector(n), x
"""

parsed_prob = """DCP problem:
    minimize norm(x)
    subject to
        x + -1*1 == 0
        x + -1*1 <= 0
  DCP objective: minimize norm(x)
    QC_norm: convex, positive, Scalar()
      Variable: affine, neither, Vector(n), x
  DCP constraints:
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

canon_prob = """DCP problem:
    minimize _t0
    subject to
        -1 + x == 0
        -1 + x <= 0
        norm([x]) <= _t0
  DCP objective: minimize _t0
    Variable: affine, neither, Scalar(), _t0
  DCP constraints:
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
        assert(str(c) == str(e))

def test_unknown_sense_error():
    """ Check that unknown sense throws an error.
    """
    yield assert_raises, Exception, ast.ProgramObjective, "hello", x

def test_dcp_error():
    """ Check that problem throws error when nonscalar objective given.
    """
    yield assert_raises, QC_DCPError, ast.ProgramObjective, "find", x

def test_non_node_objective():
    """ Check that objective throws error when objective expression is not a
        Node.
    """
    yield assert_raises, AssertionError, ast.ProgramObjective, "find", 3

def test_objective():
    """ Check that objecive's DCP compliance is properly determined.
    """
    yield check, ast.ProgramObjective("minimize", normx), exp_result % ("", "minimize")
    yield check, ast.ProgramObjective("maximize", normx), exp_result % ("Non-", "maximize")

def test_add_constraint():
    """ Check that redundant constraints are removed.
    """
    objective = ast.ProgramObjective("maximize", normx)
    local_prob = ast.SOCP(objective, ast.ProgramConstraints([]), ast.ProgramData())
    yield check_constr, local_prob, [objective]

    # checks that if you add multiple constraints, removes duplicates
    p1 = x == e.Number(1)
    p2 = x <= e.Number(2)
    p3 = e.Number(2) >= x
    
    exp_constraints = ast.ProgramConstraints([p1, p2])
    local_prob.constraints.add(p1)
    local_prob.constraints.add(p2)
    local_prob.constraints.add(p3)

    yield check_constr, local_prob, [objective,exp_constraints]

def test_program():
    """ Check the parse tree of a parsed problem and its canonical form.
    """
    yield check, prob, parsed_prob
    prob.canonicalize()
    yield check, prob, canon_prob
