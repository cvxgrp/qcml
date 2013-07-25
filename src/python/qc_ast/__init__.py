from qc_shape import Shape, Scalar, Vector, Matrix, isvector, ismatrix, isscalar
from qc_sign import Positive, Negative, Neither, ispositive, isnegative
from qc_vexity import Constant, Affine, Convex, Concave, Nonconvex, \
    isconvex, isconcave, isaffine, isconstant, \
    increasing, decreasing, nonmonotone
#from qc_expression import
from qc_ast import Objective, RelOp, Program, \
    SOC, SOCProd
from qc_expression import isparameter, isnumber, isadd, ismul, \
    Number, Parameter, Variable, \
    Add, Negate, Mul, Transpose, Slice, \
    ToVector, ToMatrix, Atom, Sum, Norm, Abs

from ast import Node, NodeVisitor, NodeTransformer
