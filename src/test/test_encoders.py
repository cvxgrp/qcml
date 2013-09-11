from qcml.codes.code import *
from qcml.codes.coefficients import *
from qcml.codes.encoders import toPython
import scipy.sparse as sp
import numpy as np
import itertools

# TODO: add test cases here

"""
CVXOPT data structures....
python_objects = [
    (ConstantCoeff(3.2), '3.2'),
    (OnesCoeff(3, ConstantCoeff(-2.1)), "o.matrix(-2.1,(3,1), tc='d')"),
    (NegateCoeff(ConstantCoeff(2)), "-(2)"),
    (EyeCoeff(3, ConstantCoeff(4.3)), "o.spmatrix(4.3,range(3),range(3), tc='d')"),
    (TransposeCoeff(ParameterCoeff('A')), "(params['A']).trans()"),
    (ParameterCoeff('A'), "params['A']"),
    (ScalarParameterCoeff('c'), "params['c']"),
    (Just(5), "[5]"),
    (LoopRows(ParameterCoeff('A'), 3, 2), "(3 + 2*idx for idx in params['A'].I)"),
    (LoopCols(ParameterCoeff('A'), 2, 3), "(2 + 3*idx for idx in params['A'].J)"),
    (LoopOver(ParameterCoeff('A'), ''), "(v for v in params['A'].V)"),
    (Range(3, 5, 2), "xrange(3, 5, 2)"),
    (Repeat(ScalarParameterCoeff('h'), 6), "itertools.repeat(params['h'], 6)"),
    (Repeat("result", 5), "itertools.repeat(result, 5)"),
    (Assign("result", AddCoeff(ParameterCoeff('A'), ParameterCoeff('B'))), "result = o.sparse(params['A'] + params['B'])")
]
"""

params = {'A':sp.coo_matrix([[1.,0,3],[4,5,0]]), 'B':np.matrix([[0.,2,0.],[0,0,6.]]), 'h':7, 'c':2.3}
elem = 2.3

python_objects = [
    (ConstantCoeff(3.2), '3.2', 3.2),
    (OnesCoeff(3, ConstantCoeff(-2.1)), "-2.1 * np.ones((3,))", 3*[-2.1]),
    (NegateCoeff(ConstantCoeff(2)), "-(2)", -2),
    (EyeCoeff(3, ConstantCoeff(4.3)), "4.3 * sp.eye(3,format='coo')", np.array([[4.3,0,0],[0,4.3,0],[0,0,4.3]])),
    (TransposeCoeff(ParameterCoeff('A')), "(params['A']).T", np.array([[1.,4.],[0.,5.], [3.,0.]])),
    (ParameterCoeff('A'), "params['A']", np.array([[1.,0,3],[4.,5.,0]])),
    (ScalarParameterCoeff('c'), "params['c']", 2.3),
    (Just(5), "[5]", [5]),
    (LoopRows(ParameterCoeff('A'), 3, 2), "(3 + 2*idx for idx in params['A'].row)", [3,3,5,5]),
    (LoopCols(ParameterCoeff('A'), 2, 3), "(2 + 3*idx for idx in params['A'].col)", [2,8,2,5]),
    (LoopOver(ParameterCoeff('A')), "(v for v in params['A'].data)",[1,3,4,5]),
    (LoopOver(ParameterCoeff('A'),"1 + 2*%s"), "(1 + 2*v for v in params['A'].data)",[3,7,9,11]),
    (LoopOver(LoopOver(ParameterCoeff('A')), "-%s"), "(-v for v in params['A'].data)", [-1,-3,-4,-5]),
    (Range(3, 6, 2), "xrange(3, 6, 2)", [3,5]),
    (Repeat(ScalarParameterCoeff('h'), 6), "itertools.repeat(params['h'], 6)", 6*[7]),
    (Repeat("elem", 5), "itertools.repeat(elem, 5)", 5*[2.3]),
    (Assign("tmp", AddCoeff(ParameterCoeff('A'), ParameterCoeff('B'))), "tmp = sp.coo_matrix(params['A'] + params['B'])", np.array([[1.,2,3],[4,5,6]])),
    (NNZ(ParameterCoeff('A')), "params['A'].nnz", 4)
]


# create a bunch of code objects
# make sure the python, C, matlab encoder prints what you expect

def check(obj, exp):
    # check that the code generator produces the expected code
    code = toPython(obj)
    print code
    print exp
    assert (code == exp)

def check_py_exec(obj, exp):
    # check that the expected code actually works in python
    code = toPython(obj)
    if isinstance(obj, Assign):
        exec(code)
        result = tmp
    else:
        result = eval(code)

    print result
    print exp
    
    if sp.issparse(result):
        assert (result.todense() == exp).all()
    elif isinstance(result, (int,float)):
        assert result == exp
    else:
        assert list(result) == exp
    
def test_encoders():
    for obj, exp, result in python_objects:
        yield check, obj, exp
        yield check_py_exec, obj, result

