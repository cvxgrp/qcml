from .. ast import ProgramObjective
from .. ast.expressions import expression
from .. ast.atoms import atom
from nose.tools import assert_raises

x = e.Variable('x')
normx = atom.QC_norm(x)
prob = Problem(Objective("minimize", normx), [x == e.Number(1), e.Number(1) >= x, x <= e.Number(1)])


# check if DCP
# check if not DCP
# check asserts