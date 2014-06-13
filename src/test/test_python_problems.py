sum_lp = """
variable x(2)
minimize sum(2*x)
"""

def python_parse_and_solve(prob):
    from .. qc_lang import QCML
    p = QCML(debug=True)
    p.parse(prob)
    p.solve()

def test_solves():
    yield python_parse_and_solve, sum_lp