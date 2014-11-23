from constraint import Constraint
from ... properties import curvature

class LinearConstraint(Constraint):
    """ expr == 0 or expr <= 0
    """
    def __init__(self, op, linear_component, shape, is_dcp):
        self.op = op
        super(LinearConstraint, self).__init__(linear_component, None, shape, is_dcp)

    def __str__(self):
        constr = "%s %s 0" % (self.left, self.op)
        if self.dual_var:
            return self.dual_var + " : " + constr
        else:
            return constr

    def canonicalize(self):
        self.left, lh_constr = self.left.canonicalize()
        return (None, [self] + lh_constr)

    def simplify(self):
        self.left = self.left.simplify()
        return self

class LinearInequality(LinearConstraint):
    """ expr <= expr
    """
    def __init__(self, left, right):
        is_dcp = curvature.isconvex(left) and curvature.isconcave(right)
        super(LinearInequality, self).__init__('<=', left-right, left.shape+right.shape, is_dcp)

class LinearEquality(LinearConstraint):
    def __init__(self, left, right):
        is_dcp = curvature.isaffine(left) and curvature.isaffine(right)
        super(LinearEquality, self).__init__('==', left-right, left.shape+right.shape, is_dcp)

# class GEQ(LinearInequality):
#     def __init__(self, left, right):
#         super(GEQ, self).__init__(right, left)
#
# class LEQ(LineqInequality):
#     def __init__(self, left, right):
#         super(LEQ, self).__init__(left, right)
