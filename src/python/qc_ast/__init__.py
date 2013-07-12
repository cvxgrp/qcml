from qc_shape import Shape, scalar, vector, matrix, isvector, ismatrix, isscalar
from qc_sign import positive, negative, neither, ispositive, isnegative
from qc_vexity import Affine, Convex, Concave, Nonconvex, \
    isconvex, isconcave, isaffine, \
    increasing, decreasing, nonmonotone
#from qc_expression import
from qc_ast import Objective, RelOp, Program, Node, NodeVisitor, NodeTransformer, \
    SOC, SOCProd
from qc_expression import isparameter, isconstant, isadd, ismul, \
    Constant, Parameter, Variable, \
    Add, Negate, Mul, Transpose, Slice, \
    ToVector, ToMatrix, Atom, Sum, Norm, Abs, Expression
