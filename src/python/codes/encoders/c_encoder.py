from encoder import create_encoder
from qcml.codes.coefficients import *

from encoder import create_encoder
from qcml.codes.coefficients import *
from qcml.codes.code import *

""" In this file, you'll often see strings with "%%(ptr)s"; this is to delay
    the evaluation of the ptr string until after the object has been turned
    into a string. Effectively, an object will be turned into a *format*
    string, instead of a string directly.
    
    TODO: this file is somewhat of a mess. but hopefully the new "parameter"
    or coefficient approach will fix this.
"""

def constant(x):
    return str(x.value)

eye = NotImplemented

def ones(x):
    return "%s" % toC(x.coeff)

trans = NotImplemented

def scalar_parameter(x):
    return "params->%s" % x.value

def parameter(x):
    return "params->%s" % x.value

def negate(x):
    return "-%s" % (toC(x.arg))

def add(x):
    raise Exception("Add not implemented.... %s + %s" % (x.left, x.right))

def mul(x):
    raise Exception("Multiply not implemented.... %s * %s" % (x.left, x.right))

def just(elem):
    return "*%%(ptr)s++ = %s;" % (elem.x)

def loop(ijv):
    def to_str(x):
        matrix = toC(x.matrix)
        if hasattr(x, 'offset') and hasattr(x, 'stride'):
            if x.offset == 0 and x.stride == 1:
                s = "for(i = 0; i < %(matrix)s->nnz; ++i) *%%(ptr)s++ = %(matrix)s->%(ijv)s[i];"
            else:
                s = "for(i = 0; i < %(matrix)s->nnz; ++i) *%%(ptr)s++ = %(offset)d + %(stride)d*(%(matrix)s->%(ijv)s[i]);"
            return  s % ({'matrix': matrix, 'offset': x.offset, 'stride': x.stride, 'ijv': ijv})
        return "for(i = 0; i < %s->nnz; ++i) *%%(ptr)s++ = %s;" % (matrix, x.op % ("%s->%s[i]" % (matrix, ijv)))
    return to_str

def _range(x):
    if x.stride == 1:
        return "for(i = %d; i < %d; ++i) *%%(ptr)s++ = i;" % (x.start, x.end)
    else:
        return "for(i = %d; i < %d; i+=%d) *%%(ptr)s++ = i;" % (x.start, x.end, x.stride)

def repeat(x):
    return "for(i = 0; i < %d; ++i) *%%(ptr)s++ = %s;" % (x.n, toC(x.obj))

def assign(x):
    raise Exception("Assignment not implemented.... %s = %s" % (x.lhs, x.rhs))

def nnz(x):
    return "%s->nnz" % (toC(x.obj))
    
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
    LoopRows: loop("i"),
    LoopCols: loop("j"),
    LoopOver: loop("v"),
    Range: _range,
    Repeat: repeat,
    Assign: assign,
    NNZ: nnz,
    str: lambda x: x,
    int: lambda x: str(x)
}

toC = create_encoder(lookup)
