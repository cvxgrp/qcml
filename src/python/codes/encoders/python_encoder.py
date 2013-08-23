from encoder import create_encoder
from qcml.codes.coefficients import *

def constant(x):
    return str(x.value)

def eye(x):
    return "o.spmatrix(%s,range(%s),range(%s), tc='d')" % (toPython(x.coeff), x.n, x.n)

def ones(x):
    if x.transpose: return "o.matrix(%s,(1,%s), tc='d')" % (toPython(x.coeff), x.n)
    else: return "o.matrix(%s,(%s,1), tc='d')" % (toPython(x.coeff), x.n)

def trans(x):
    return "(%s).trans()" % toPython(x.arg)

def scalar_parameter(x):
    return "params['%s']" % x.value

def parameter(x):
    return "o.matrix(params['%s'])" % x.value

def negate(x):
    return "-(%s)" % toPython(x.arg)

def add(x):
    return "%s + %s" % (toPython(x.left), toPython(x.right))

def mul(x):
    return "%s*%s" % (toPython(x.left), toPython(x.right))
    
lookup = {
    ConstantCoeff: constant,
    OnesCoeff: ones,
    NegateCoeff: negate,
    EyeCoeff: eye,
    TransposeCoeff: trans,
    ParameterCoeff: parameter,
    ScalarParameterCoeff: scalar_parameter,
    AddCoeff: add,
    MulCoeff: mul,
    basestring: lambda x: x
}

toPython = create_encoder(lookup)

# TODO: Single, LoopRows, LoopCols, Loop, Range, Repeat, Assign
        
# 
# class ConstantCoeff(CoeffExpr):
#     def __init__(self, value):
#         self.value = value
#         self.isknown = True
#         self.isscalar = True
#         self.is_matrix_param = False
# 
#     def I(self, row_offset, stride=1): return Single(row_offset)
#     def J(self, col_offset, stride=1): return Single(col_offset)
#     def V(self): return Single(self.value)
# 
#     def __str__(self): return str(self.value)
# 
# class ParameterCoeff(CoeffExpr):
#     def __init__(self, value):
#         self.value = value
#         self.isknown = True
#         self.isscalar = False
#         self.is_matrix_param = True
# 
#     def to_sparse(self): return "params['%s'] = o.sparse(%s)" % (self.value, self)
#     def I(self, row_offset, stride=1): return LoopRows(self.value, )"(%d + %d*i for i in params['%s'].I)" % (row_offset, stride, self.value)
#     def J(self, col_offset, stride=1): return LoopCols()"(%d + %d*j for j in params['%s'].J)" % (col_offset, stride, self.value)
#     def V(self): return Loop(self.value) #"(v for v in params['%s'].V)" % self.value
# 
#     def __str__(self): return self.value
# 
# class ScalarParameterCoeff(ParameterCoeff):
#     def __init__(self,value):
#         super(ScalarParameterCoeff, self).__init__(value)
#         self.isscalar = True
#         self.is_matrix_param = False
# 
#     def I(self, row_offset, stride=1): return Single(row_offset)
#     def J(self, col_offset, stride=1): return Single(row_offset)
#     def V(self): return Single(self.value)
# 
# class NegateCoeff(CoeffExpr):
#     def __init__(self, arg):
#         self.arg = arg
#         self.isknown = arg.isknown
#         self.isscalar = arg.isscalar
#         self.is_matrix_param = arg.is_matrix_param
# 
#     def to_sparse(self): return self.arg.to_sparse()
#     def I(self, row_offset, stride=1): return self.arg.I(row_offset, stride)
#     def J(self, col_offset, stride=1): return self.arg.J(col_offset, stride)
#     def V(self): return Loop(self.arg)#"(-x for x in %s)" % self.arg.V()
# 
#     def __str__(self): return "-(%s)" % self.arg
# 
# class EyeCoeff(CoeffExpr):
#     def __init__(self, n, coeff):
#         self.n = n
#         self.coeff = coeff
#         self.isknown = True
#         self.isscalar = False
#         self.is_matrix_param = False
# 
#     def I(self, row_offset, stride=1): return Range(row_offset, row_offset + stride*self.n, stride)
#     def J(self, col_offset, stride=1): return Range(col_offset, col_offset + stride*self.n, stride)
#     def V(self): return Repeat(self.coeff, self.n)
# 
#     # def __str__(self): return "_o.spmatrix(%s,range(%s),range(%s), tc='d')" % (self.coeff, self.n, self.n)
# 
# class OnesCoeff(CoeffExpr):
#     def __init__(self, n, coeff, transpose = False):
#         self.n = n
#         self.coeff = coeff
#         self.transpose = transpose
#         self.isknown = True
#         self.isscalar = False
#         self.is_matrix_param = False
# 
#     def I(self, row_offset, stride=1):
#         if self.transpose:
#             return Repeat(row_offset, self.n)
#         else:
#             return Range(row_offset, row_offset + stride*self.n, stride)
# 
#     def J(self, col_offset, stride=1):
#         if self.transpose:
#             return Range(col_offset, col_offset + stride*self.n, stride)
#         else:
#             return Repeat(col_offset, self.n)
# 
#     def V(self): return Repeat(self.coeff, self.n)
# 
#     # def __str__(self):
#     #     if self.transpose:
#     #         return "_o.matrix(%s,(1,%s), tc='d')" % (self.coeff, self.n)
#     #     else:
#     #         return "_o.matrix(%s,(%s,1), tc='d')" % (self.coeff, self.n)
# 
# class AddCoeff(CoeffExpr):
#     def __init__(self, left, right):
#         self.left = left
#         self.right = right
#         self.isknown = left.isknown and right.isknown
#         self.isscalar = left.isscalar and right.isscalar
#         self.is_matrix_param = left.is_matrix_param or right.is_matrix_param
# 
#     def to_sparse(self): return "result = o.sparse(%s + %s)" % (str(self.left), str(self.right))
#     def I(self, row_offset, stride=1): return "(%d + %d*i for i in result.I)" % (row_offset, stride)
#     def J(self, col_offset, stride=1): return "(%d + %d*j for j in result.J)" % (col_offset, stride)
#     def V(self): return Loop("result")
# 
#     def __str__(self): return "%s + %s" % (self.left, self.right)
# 
# class MulCoeff(CoeffExpr):
#     def __init__(self, left, right):
#         self.left = left
#         self.right = right
#         self.isknown = left.isknown and right.isknown
#         self.isscalar = left.isscalar and right.isscalar
#         self.is_matrix_param = left.is_matrix_param or right.is_matrix_param
# 
#     def to_sparse(self): return "result = o.sparse(%s * %s)" % (str(self.left), str(self.right))
#     def I(self, row_offset, stride=1): return "(%d + %d*i for i in result.I)" % (row_offset, stride)
#     def J(self, col_offset, stride=1): return "(%d + %d*j for j in result.J)" % (col_offset, stride)
#     def V(self): return Loop("result")
# 
#     def __str__(self): return "%s * %s" % (self.left, self.right)
# 
# 
