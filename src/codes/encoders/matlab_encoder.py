from encoder import create_encoder
from qcml.codes.coefficients import *
from qcml.codes.code import *

def constant(x):
    return str(x.value)

def eye(x):
    return "%s * speye(%d)" % (toMatlab(x.coeff), x.n)

def ones(x):
    if x.transpose: return "%s * ones(1,%d)" % (toMatlab(x.coeff), x.n)
    else:           return "%s * ones(%d,1)" % (toMatlab(x.coeff), x.n)

def trans(x):
    return "(%s).'" % toMatlab(x.arg)

def parameter(x):
    return "params.%s" % x.value

def scalar_parameter(x):
    return "params.%s" % x.value

def negate(x):
    return "-(%s)" % toMatlab(x.arg)

def add(x):
    return "%s + %s" % (toMatlab(x.left), toMatlab(x.right))

def mul(x):
    return "dot(%s,%s)" % (toMatlab(x.left), toMatlab(x.right))

def just(elem):
    # FIXME
    return "just(%s)" % elem.x

def loop(ijv):
    # FIXME
    def to_str(x):
        return "loop %s" % ijv
    return to_str

def _range(x):
    return "%d:%d:%d" % (x.start, x.stride, x.end)

def repeat(x):
    return "repmat(%s,1,%d)" % (toMatlab(x.obj), x.n)

def assign(x):
    return "%s = sparse(%s)" % (toMatlab(x.lhs), toMatlab(x.rhs))

def nnz(x):
    return "%s.nnz" % (toMatlab(x.obj))


lookup = {
    ConstantCoeff:          constant,
    EyeCoeff:               eye,
    OnesCoeff:              ones,
    TransposeCoeff:         trans,
    ParameterCoeff:         parameter,
    ScalarParameterCoeff:   parameter,
    NegateCoeff:            negate,
    AddCoeff:               add,
    MulCoeff:               mul,
    Just:                   just,
    LoopRows:               loop("row"),
    LoopCols:               loop("col"),
    LoopOver:               loop("data"),
    Range:                  _range,
    Repeat:                 repeat,
    Assign:                 assign,
    NNZ:                    nnz,
    str: lambda x: x,
    int: lambda x: str(x)
}
toMatlab = create_encoder(lookup)
