from qcml.codes.code import *
from qcml.codes.coefficients import *
from qcml.codes.encoders import toPython

# TODO: add test cases here

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
    (LoopOver(ParameterCoeff('A')), "(v for v in params['A'].V)"),
    (Range(3, 5, 2), "xrange(3, 5, 2)"),
    (Repeat(ScalarParameterCoeff('h'), 6), "itertools.repeat(params['h'], 6)"),
    (Repeat("result", 5), "itertools.repeat(result, 5)"),
    (Assign("result", AddCoeff(ParameterCoeff('A'), ParameterCoeff('B'))), "result = o.sparse(params['A'] + params['B'])")
]
# create a bunch of code objects
# make sure the python, C, matlab encoder prints what you expect

def check(obj, exp):
    result = toPython(obj)
    print result
    print exp
    assert (result == exp)
    
def test_encoders():
    for obj, exp in python_objects:
        yield check, obj, exp

