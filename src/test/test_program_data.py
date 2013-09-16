from .. import ast
from nose.tools import eq_

def check(exp):
    assert(exp)
    
def test_default_data():
    data = ast.ProgramData()
    yield eq_, data.dimensions, set()
    yield eq_, data.parameters, {}
    yield eq_, data.variables, {}

def test_data_iterator():
    dims = set({'m', 'n'})
    variables = {'x':1, 'y':2}
    parameters = {'a':1, 'b':2}
    data = ast.ProgramData(dims, parameters, variables)
    
    expected = ['m', 'n', 'a', 'b', 'y', 'x']
    for exp, elem in zip(expected,data):
        yield eq_, elem, exp