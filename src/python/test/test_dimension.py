from scoop.qc_ast import Dimension
import operator

def eq_dimension(dimension, lookup, expected):
    Dimension.lookup = lookup
    assert dimension == expected
    Dimension.lookup = None

def test_equalities():
    eq_list = [
        (Dimension('1'), None, 1),
        (Dimension('1'), None, '1'),
        (Dimension('1'), None, Dimension(1)),
        (Dimension('n'), None, 'n'),
        (Dimension('n'), {'n':1}, '1'),
        (Dimension('n'), {'n':1}, 1),
        (Dimension('n'), {'n':1}, Dimension(1))
    ]
    for dimension,lookup,expected in eq_list:
        yield eq_dimension, dimension, lookup, expected

def test_add():
    add_list = [
        (Dimension('1') + Dimension('2'), None, 3),
        (Dimension('n') + Dimension('2'), None, 'n + 2'),
        (Dimension('n') + Dimension('n') + Dimension('m'), None, '2*n + m'),
        (Dimension('n') + Dimension('n') + Dimension('m'), {'n':2, 'm':3}, 7)
    ]
    for dimension,lookup,expected in add_list:
        yield eq_dimension, dimension, lookup, expected