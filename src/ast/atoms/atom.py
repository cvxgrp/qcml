from .. expressions import Expression
from ... properties import sign, curvature

atoms = {}

class Atom(Expression):
    """ Atom AST node.

        Stores the name of the atom and its arguments.

        Base class for implementing atoms
    """

    def __init__(self,*args):
        self.args = args    # list of Expression arguments
        super(Atom, self).__init__(self.eval_curvature(), self._shape(), self._sign())

    def __str__(self):
        name = self.__class__.__name__.strip('QC_')
        return "%s(%s)" % (name, ', '.join([str(arg) for arg in self.args]))

    def eval_curvature(self):
        vexity = self._curvature()
        for elem, func in zip(self.args, self._monotonicity()):
            vexity += func(elem)
        return vexity

    def _monotonicity(self):
        # monotonicity in each argument
        return NotImplemented

    def _curvature(self):
        # base curvature
        return NotImplemented

    def _sign(self):
        # base sign
        return NotImplemented

    def _shape(self):
        # base shape
        return NotImplemented

    def _canonicalize(self):
        # specific to each atom
        return NotImplemented

    def canonicalize(self):
        # basically borrowed from CVXPY

        # TODO: need to take care of what happens when it's
        #   expr + convex_atom(expr) <= expr
        #   expr + concave_atom(expr) >= expr
        # in these cases, if it's possible (i.., canonicalizes to a single
        # epigraph variable), replace the epigraph variable with the linear
        # expression
        #
        self.args, constraints = zip(*[elem.canonicalize() for elem in self.args])

        # canonicalize self
        base_obj, base_constraints = self._canonicalize()
        # obj is now a synonym for the atom expression

        # canonicalize constraints
        # only take the second component, since first is None
        # seed initial constrs with constraints from canonicalizing subexpressions
        constrs = []
        for elem in constraints: constrs.extend(elem)
        for constr in base_constraints:
            if constr: constrs += constr.canonicalize()[1]
        return (base_obj, constrs)

    # we only call simplify *after* we call canonicalize
    def simplify(self):
        self.args = [elem.simplify() for elem in self.args]
        return self

    def children(self):
        for e in self.args:
            if e is not None: yield e

from qc_abs import *
from qc_geo_mean import *
from qc_huber import *
from qc_inv_pos import *
from qc_max import *
from qc_min import *
from qc_neg import *
from qc_norm_inf import *
from qc_norm import *
from qc_norm1 import *
from qc_pos import *
from qc_pow_rat import *
from qc_quad_over_lin import *
from qc_sqrt import *
from qc_square_over_lin import *
from qc_square import *
