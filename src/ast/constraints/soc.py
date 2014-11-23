from constraint import Constraint
from ... properties import curvature, shape

class SOCConstraint(Constraint):
    """ norm([left]) <= right or norms(left) <= right
    """
    def __init__(self, left, right, shape):
        is_dcp = all(map(curvature.isaffine, left)) and curvature.isaffine(right)
        super(SOCConstraint, self).__init__(left, right, shape, is_dcp)

    def canonicalize(self):
        self.left, constraints = map(list, zip(*[elem.canonicalize() for elem in self.left]))
        self.right, constraint = self.right.canonicalize()
        constr = constraint
        for c in constraints:
            constr += c
        return (None, [self] + constr)

    def simplify(self):
        self.left = [elem.simplify() for elem in self.left]
        self.right = self.right.simplify()
        return self


class SOC(SOCConstraint):
    """ SOC AST node.

        This node forms a second-order cone constraint in the program.

        norm([x;y;z]) <= t

        It is DCP compliant when x,y,z, and t are Affine expressions.
    """
    def __init__(self, t, args):
        if not shape.isscalar(t):
            raise TypeError("Cannot form SOC constraint with vector '%s' right-hand side." % t)
        super(SOC, self).__init__(args, t, shape.Scalar())

    def __str__(self):
        constr = "norm([%s]) <= %s" % ('; '.join(map(str,self.left)), self.right)
        if self.dual_var:
            return self.dual_var + " : " + constr
        else:
            return constr

class SOCProd(SOCConstraint):
    """ SOCProd AST node.

        This node forms a product of second-order cone constraints in the
        program.

        norm([x1;y1;z1]) <= t1
        norm([x2;y2;z2]) <= t2
        ...

        More concisely, it is written as

        norm(x,y,z) <= t

        where x = (x1,x2,..), y = (y1,y2,..), and so on

        It is DCP compliant when the arguments and t are Affine expressions.
    """
    def __init__(self, t, args):
        my_shape = sum(map(lambda x: x.shape, args), t.shape)
        self.nargs = len(args)
        super(SOCProd, self).__init__(args, t, my_shape)


    def __str__(self):
        if self.nargs == 1:
            constr = "abs(%s) <= %s" % (self.left[0], self.right)
            if self.dual_var:
                return self.dual_var + " : " + constr
            else:
                return constr
        else:
            constr = "norm(%s) <= %s" % (', '.join(map(str, self.left)), self.right)
            if self.dual_var:
                return self.dual_var + " : " + constr
            else:
                return constr
