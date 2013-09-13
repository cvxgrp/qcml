""" Everything in here goes with zero indexing.  It could have been one indexed,
    except I wasn't sure if it was okay to make Just return 1 more than its arg.
    Probably better not to do that.  So I left everything here 0 indexed and 
    then add 1 to the Gi Gj Ai Aj when converting to compressed sparse format
    in functions_return
"""

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
    return "%s" % elem.x

def loop_rows(x):
    mat = toMatlab(x.matrix)
    ret = "mod(find(%s)-1,size(%s,1))" % (mat, mat)

    if hasattr(x, 'stride') and x.stride != 1:
        ret = "%d*%s" % (x.stride, ret)
    if hasattr(x, 'offset') and x.offset != 0:
        ret = "%d + %s" % (x.offset, ret)

    return ret

def loop_cols(x):
    mat = toMatlab(x.matrix)
    ret = "floor((find(%s)-1)/size(%s,1))" % (mat, mat)

    if hasattr(x, 'stride') and x.stride != 1:
        ret = "%d*%s" % (x.stride, ret)
    if hasattr(x, 'offset') and x.offset != 0:
        ret = "%d + %s" % (x.offset, ret)

    return ret

def loop_over(x):
    return "nonzeros(%s)" % (x.op % toMatlab(x.matrix))

def _range(x):
    if x.stride == 1: return "(%d:%d)'" % (x.start, x.end-1)
    else:             return "(%d:%d:%d)'" % (x.start, x.stride, x.end-1)

def repeat(x):
    return "repmat(%s,%d,1)" % (toMatlab(x.obj), x.n)

def assign(x):
    # FIXME
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
    LoopRows:               loop_rows,
    LoopCols:               loop_cols,
    LoopOver:               loop_over,
    Range:                  _range, # "range" is reserved
    Repeat:                 repeat,
    Assign:                 assign,
    NNZ:                    nnz,
    str: lambda x: x,
    int: lambda x: str(x)
}
toMatlab = create_encoder(lookup)
