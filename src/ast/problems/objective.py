from .. node import Node
from .. expressions import expression as e
from qcml.errors import QC_DCPError
from qcml.properties import shape, curvature
import sys

class Objective(Node):
    """ Objective node.

        This node contains the representation of the objective. It also stores
        the objective's sense (minimize, maximize, or find).

        It is DCP compliant when the objective's expression agrees with its
        sense. Convex for minimization, concave for maximization, or affine
        for find / feasibility problems.
    """
    check_dcp = {
        'minimize': curvature.isconvex,
        'maximize': curvature.isconcave,
        'find': curvature.isaffine
    }
    def __init__(self, sense, expr):
        # for debugging, ensure that objectives are canonicalizable and showable
        assert(isinstance(expr, (Node, e.Expression)))   
        if not shape.isscalar(expr): raise QC_DCPError("Objective is not scalar")

        self.sense = sense
        self.expr = expr
        try:
            self.is_dcp = Objective.check_dcp[sense](expr)
        except KeyError:
            raise Exception("Unrecognized problem sense '%s'" % sense)
        
    def __str__(self): return "%s %s" % (self.sense, self.expr)

    def info(self):
        if self.is_dcp: return "DCP objective: %s" % (self,)
        else: return "Non-DCP objective: %s" % (self,)
    
    def children(self):
        yield self.expr
        
    def canonicalize(self):
        obj, constraints = self.expr.canonicalize()
        self.expr = obj.simplify()
        return (obj, constraints)

