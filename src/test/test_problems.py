import numpy as np
sum_lp = """
variable x(2)
minimize sum(2*x)
"""

sum_mat_lp = """
variable x(2)
parameter D(2,2)
minimize sum(D*x)
"""

def python_parse_and_solve(prob):
    from .. qc_lang import QCML
    p = QCML(debug=True)
    p.parse(prob)
    D = np.eye(2)
    p.solve()

def C_parse_and_codegen(prob):
    from .. qc_lang import QCML
    p = QCML(debug=True)
    p.parse(prob)
    p.canonicalize()
    p.codegen("C")

def test_solves():
    yield python_parse_and_solve, sum_lp
    yield C_parse_and_codegen, sum_lp
    yield python_parse_and_solve, sum_mat_lp
    yield C_parse_and_codegen, sum_mat_lp