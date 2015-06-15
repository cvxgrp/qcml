from encoder import create_encoder
from ... import codes
from ... properties.abstract_dim import AbstractDim

def constant(x):
    return str(x.value)

def eye(x):
    return "%s * sp.eye(%s,%s,format='coo')" % (toPython(x.coeff), x.n, x.n)

def ones(x):
    if x.transpose: return "%s * np.ones((1,%s))" % (toPython(x.coeff), x.n)
    else: return "%s"" * np.ones((%s,))" % (toPython(x.coeff), x.n)

def trans(x):
    return "(%s).T" % toPython(x.arg)

def scalar_parameter(x):
    return "params['%s']" % x.value

def parameter(x):
    return "params['%s']" % x.value

def negate(x):
    return "-(%s)" % toPython(x.arg)

def add(x):
    return "%s + %s" % (toPython(x.left), toPython(x.right))

def mul(x):
    if x.left.is_matrix_param:
        return "%(lhs)s.dot(%(rhs)s)" % {'lhs':toPython(x.left), 'rhs': toPython(x.right)}
    else:
        return "%(lhs)s * %(rhs)s" % {'lhs':toPython(x.left), 'rhs': toPython(x.right)}
    #return "%(lhs)s.dot(%(rhs)s) if (isinstance(%(lhs)s, np.ndarray) and isinstance(%(rhs)s, np.ndarray)) else %(lhs)s * %(rhs)s" % {'lhs':toPython(x.left), 'rhs': toPython(x.right)}

def just(elem):
    return "[%s]" % toPython(elem.x)

def loop(ijv):
    def to_str(x):
        matrix = toPython(x.matrix)
        if hasattr(x, 'offset') and hasattr(x, 'stride'):
            if x.offset == 0 and x.stride == 1:
                return "(idx for idx in %s.%s)" % (matrix, ijv)
            return "(%s + %s*idx for idx in %s.%s)" % (x.offset, x.stride, matrix, ijv)
        return "(%s for v in %s.%s)" % (x.op % "v", matrix, ijv)
    return to_str

def _range(x):
    return "xrange(%s, %s, %s)" % (x.start, x.end, x.stride)

def repeat(x):
    return "itertools.repeat(%s, %s)" % (toPython(x.obj), x.n)

def assign(x):
    if isinstance(x.lhs, codes.TransposeCoeff):
        lhs = toPython(x.lhs.arg)
    else:
        lhs = toPython(x.lhs)

    if isinstance(x.rhs, codes.TransposeCoeff):
        rhs = toPython(x.rhs.arg)
        return "%s = sp.coo_matrix(%s.reshape((%s,%s))) if not sp.isspmatrix_coo(%s) else %s" % (lhs, rhs, x.rhs.arg.rows, x.rhs.arg.cols, rhs, rhs)
    else:
        rhs = toPython(x.rhs)
        return "%s = sp.coo_matrix(%s)" % (lhs, rhs)

def nnz(x):
    return "%s.nnz" % (toPython(x.obj))

lookup = {
    codes.ConstantCoeff:            constant,
    codes.OnesCoeff:                ones,
    codes.NegateCoeff:              negate,
    codes.EyeCoeff:                 eye,
    codes.TransposeCoeff:           trans,
    codes.ParameterCoeff:           parameter,
    codes.ScalarParameterCoeff:     scalar_parameter,
    codes.AddCoeff:                 add,
    codes.MulCoeff:                 mul,
    codes.Just:                     just,
    codes.LoopRows:                 loop("row"),
    codes.LoopCols:                 loop("col"),
    codes.LoopOver:                 loop("data"),
    codes.Range:                    _range, # "range" is reserved
    codes.Repeat:                   repeat,
    codes.Assign:                   assign,
    codes.NNZ:                      nnz,
    str:                            lambda x: x,
    int:                            lambda x: str(x),
    float:                          lambda x: str(x),
    AbstractDim:                    lambda x: str(x)
}



toPython = create_encoder(lookup)
