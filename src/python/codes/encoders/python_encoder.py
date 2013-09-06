from encoder import create_encoder
from qcml.codes.coefficients import *
from qcml.codes.code import *

def constant(x):
    return str(x.value)

def eye(x):
    return "%s * sp.eye(%s,format='coo')" % (toPython(x.coeff), x.n)

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
    return "%s*%s" % (toPython(x.left), toPython(x.right))

def just(elem):
    return "[%s]" % elem.x

def loop(ijv):
    def to_str(x):
        if hasattr(x, 'offset') and hasattr(x, 'stride'):
            return "(%d + %d*idx for idx in %s.%s)" % (x.offset, x.stride, toPython(x.matrix), ijv)
        return "(v for v in %s.%s)" % (toPython(x.matrix), ijv)
    return to_str

def _range(x):
    return "xrange(%d, %d, %d)" % (x.start, x.end, x.stride)

def repeat(x):
    return "itertools.repeat(%s, %d)" % (toPython(x.obj), x.n)

def assign(x):
    return "%s = sp.coo_matrix(%s)" % (toPython(x.lhs), toPython(x.rhs))
    
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
    Just: just,
    LoopRows: loop("row"),
    LoopCols: loop("col"),
    LoopOver: loop("data"),
    Range: _range,
    Repeat: repeat,
    Assign: assign,
    str: lambda x: x,
    int: lambda x: str(x)
}



toPython = create_encoder(lookup)