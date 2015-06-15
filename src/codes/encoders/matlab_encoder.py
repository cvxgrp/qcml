""" Everything in here goes with zero indexing.  It could have been one indexed,
    except I wasn't sure if it was okay to make Just return 1 more than its arg.
    Probably better not to do that.  So I left everything here 0 indexed and
    then add 1 to the Gi Gj Ai Aj when converting to compressed sparse format
    in functions_return
"""

from encoder import create_encoder
from ... import codes
from ... properties.abstract_dim import AbstractDim

def constant(x):
    return str(x.value)

def eye(x):
    return "%s * speye(%s)" % (toMatlab(x.coeff), x.n)

def ones(x):
    if x.transpose: return "%s * ones(1,%s)" % (toMatlab(x.coeff), x.n)
    else:           return "%s * ones(%s,1)" % (toMatlab(x.coeff), x.n)

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
    return "%s * %s" % (toMatlab(x.left), toMatlab(x.right))

def just(elem):
    return "%s" % toMatlab(elem.x)


def loop_rows(x):
    mat = toMatlab(x.matrix)
    ret = "mod(find(%s)-1,size(%s,1))" % (mat, mat)

    if hasattr(x, 'stride') and x.stride != 1:
        ret = "%d*%s" % (x.stride, ret)
    if hasattr(x, 'offset') and x.offset != 0:
        ret = "%s + %s" % (x.offset, ret)

    return ret

def loop_cols(x):
    mat = toMatlab(x.matrix)
    ret = "floor((find(%s)-1)/size(%s,1))" % (mat, mat)

    if hasattr(x, 'stride') and x.stride != 1:
        ret = "%d*%s" % (x.stride, ret)
    if hasattr(x, 'offset') and x.offset != 0:
        ret = "%s + %s" % (x.offset, ret)

    return ret

def loop_over(x):
    return "nonzeros(%s)" % (x.op % toMatlab(x.matrix))

def _range(x):
    if x.stride == 1: return "(%s:%s)'" % (x.start, x.end-1)
    else:             return "(%s:%s:%s)'" % (x.start, x.stride, x.end-1)

def repeat(x):
    return "%s*ones(%s,1)" % (toMatlab(x.obj), x.n)

def assign(x):
    # echu: probably will be source of some massive bugs in matlab
    # see, for instance, the python_encoder
    return "%s = sparse(%s);" % (toMatlab(x.lhs), toMatlab(x.rhs))

def nnz(x):
    return "%s.nnz" % (toMatlab(x.obj))


lookup = {
    codes.ConstantCoeff:          constant,
    codes.EyeCoeff:               eye,
    codes.OnesCoeff:              ones,
    codes.TransposeCoeff:         trans,
    codes.ParameterCoeff:         parameter,
    codes.ScalarParameterCoeff:   parameter,
    codes.NegateCoeff:            negate,
    codes.AddCoeff:               add,
    codes.MulCoeff:               mul,
    codes.Just:                   just,
    codes.LoopRows:               loop_rows,
    codes.LoopCols:               loop_cols,
    codes.LoopOver:               loop_over,
    codes.Range:                  _range, # "range" is reserved
    codes.Repeat:                 repeat,
    codes.Assign:                 assign,
    codes.NNZ:                    nnz,
    str:                          lambda x: x,
    int:                          lambda x: str(x),
    float:                        lambda x: str(x),
    AbstractDim:                  lambda x: str(x)
}
toMatlab = create_encoder(lookup)
