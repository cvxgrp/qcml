from expression import Expression, Variable, Parameter, Constant, \
    CONVEX, CONCAVE, AFFINE, NONCONVEX, transpose
from shape import Scalar, Vector, Matrix
from sign import Sign, POSITIVE, NEGATIVE, UNKNOWN
from constraint import EqConstraint, LeqConstraint, GeqConstraint, Cone

from utils import iscvx, isccv, isaff, \
    ispositive, isnegative, isunknown, \
    increasing, decreasing, nonmonotone, \
    isconstant, to_scoop, isscalar, isvector, ismatrix
    
