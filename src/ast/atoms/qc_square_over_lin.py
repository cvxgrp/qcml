""" This is the square_over_lin atom.

        square_over_lin(x,y) = (x).^2 ./ y

    If y is a vector, it computes the division elementwise.

    It is a CONVEX atom. It is NONMONOTONE in the first argument, and
    DECREASING in the second argument.

    If the first argument is POSITIVE, it is INCREASING in the first argument.
    If the first argument is NEGATIVE, it is DECRASING in the first argument.

    It returns a SCALAR expression if the second argument is SCALAR.
    Otherwise, it returns a VECTOR expression (sized to match the second
    arugment).

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
import atom
from utils import *

class QC_square_over_lin(atom.Atom):
    def __init__(self, x, y):
        super(QC_square_over_lin, self).__init__(x, y)

    def _monotonicity(self):
        return [monotonicity.signed(self.args[0]), monotonicity.decreasing]

    def _curvature(self):
        return curvature.Convex()

    def _sign(self):
        return sign.Positive()

    def _shape(self):
        x,y = self.args
        if shape.isscalar(y):
            return x.shape
        elif shape.isscalar(x):
            return y.shape
        elif y.shape.row == x.shape.row:
            return x.shape
        else:
            raise TypeError("Cannot use square_over_lin with x, y disagreeing in length.")


    def _canonicalize(self):
        v = Variable('', self.shape)
        x,y = self.args
        constraints = [
            SOCProd(y + v, [y - v, Number(2.0)*x]),
            y >= Number(0)
        ]
        return (v, constraints)


# register with the atom library
atom.atoms['square_over_lin'] = QC_square_over_lin
