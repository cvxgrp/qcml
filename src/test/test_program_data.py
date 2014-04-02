from .. import ast
from .. ast.expressions import expression as e
from .. properties import shape, sign
from nose.tools import eq_


def test_default_data():
    """ Test that default arguments are set.
    """
    data = ast.ProgramData()
    yield eq_, data.dimensions, set()
    yield eq_, data.parameters, {}
    yield eq_, data.variables, {}

def test_abstract_dimension_getter():
    """ Test that abstract dimensions are properly indicated.
    """
    dims = set({'m', 'n', 1, 3})
    data = ast.ProgramData(dims)
    assert (data.abstract_dims == ['m', 'n'])

def test_data_dimension_setter():
    """ Test that dimensions are properly set and the constructor works.
    """
    dims = set({'m', 'n'})
    variables = {
        'x': e.Variable('x', shape.Scalar()),
        'y': e.Variable('y', shape.Vector('m'))
    }
    parameters = {
        'a': e.Parameter('a', shape.Matrix('m','n'), sign.Neither()),
        'b': e.Parameter('b', shape.Vector('n'), sign.Neither())
    }
    data = ast.ProgramData(dims, parameters, variables)

    yield eq_, data.dimensions, set(['m', 'n'])
    yield eq_, data.parameters.keys(), ['a', 'b']
    yield eq_, data.variables.keys(), ['y', 'x']

    data.dimensions = {'m': 5}

    def elems():
        """ Iterator for the values of parameters and variables
        """
        for elem in data.parameters.values():
            yield elem
        for elem in data.variables.values():
            yield elem

    expected_shapes = [shape.Matrix(5, 'n'), shape.Vector('n'), shape.Vector(5), shape.Scalar()]
    for expected_shape, elem in zip(expected_shapes, elems()):
        yield eq_, elem.shape, expected_shape

    yield eq_, data.dimensions, set([5, 'n'])



