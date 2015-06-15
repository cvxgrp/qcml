from encoder import create_encoder
from ... import codes
from ... properties.abstract_dim import AbstractDim

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

def trans(x):
    # if x is a TransposeCoeff with arg having col=1, then we should
    # treat it as a "dense" vector
    if isinstance(x.arg, codes.ParameterCoeff) and x.arg.cols == 1:
        return toC(x.arg)
    else:
        raise NotImplementedError

def scalar_parameter(x):
    return "params->%s" % x.value

def parameter(x):
    return "params->%s" % x.value

def negate(x):
    return "-%s" % (toC(x.arg))

def add(x):
    raise Exception("Add not implemented.... %s + %s" % (x.left, x.right))

def mul(x):
    if x.left.isscalar:
        return "%s * %s" % (toC(x.left), toC(x.right))
    else:
        raise Exception("Multiply not implemented.... %s * %s" % (x.left, x.right))

def just(elem):
    return "*%%(ptr)s++ = %s;" % toC(elem.x)

def loop(ijv):
    def to_str(x):
        matrix = toC(x.matrix)
        # only used for transposes..
        if isinstance(x.matrix, codes.TransposeCoeff):
            if ijv == "i":
                val = "0"
            elif ijv == "j":
                val = "i"
            else:
                val = "%(matrix)s[i]"
        # or for vector parameters (which are treated as dense vectors)
        if isinstance(x.matrix, codes.ParameterCoeff) and x.matrix.cols == 1:
            if ijv == "i":
                val = "i"
            elif ijv == "j":
                val = "0"
            else:
                val = "%(matrix)s[i]"
        if hasattr(x, 'offset') and hasattr(x, 'stride'):
            if x.offset == 0 and x.stride == 1:
                # we only get here if it was a transpose of a vector coeff
                if isinstance(x.matrix, codes.TransposeCoeff):
                    s = "for(i = 0; i < {length}; ++i) *%%(ptr)s++ = {value};".format(length=x.matrix.arg.rows, value = val)
                elif isinstance(x.matrix, codes.ParameterCoeff) and x.matrix.cols == 1:
                    s = "for(i = 0; i < {length}; ++i) *%%(ptr)s++ = {value};".format(length=x.matrix.rows, value = val)
                else:
                    s = "for(i = 0; i < %(matrix)s->nnz; ++i) *%%(ptr)s++ = %(matrix)s->%(ijv)s[i];"
            else:
                if isinstance(x.matrix, codes.TransposeCoeff):
                    s = "for(i = 0; i < {length}; ++i) *%%(ptr)s++ = %(offset)s + %(stride)s*({value});".format(length=x.matrix.arg.rows, value = val)
                elif isinstance(x.matrix, codes.ParameterCoeff) and x.matrix.cols == 1:
                    s = "for(i = 0; i < {length}; ++i) *%%(ptr)s++ = %(offset)s + %(stride)s*({value});".format(length=x.matrix.rows, value = val)
                else:
                    s = "for(i = 0; i < %(matrix)s->nnz; ++i) *%%(ptr)s++ = %(offset)s + %(stride)s*(%(matrix)s->%(ijv)s[i]);"
            return  s % ({'matrix': matrix, 'offset': x.offset, 'stride': x.stride, 'ijv': ijv})
        if isinstance(x.matrix, codes.TransposeCoeff):
            if ijv == "v":
                val = x.op % ("%s[i]" % matrix)
            return "for(i = 0; i < {length}; ++i) *%(ptr)s++ = {value};".format(length=x.matrix.arg.rows, value = val)
        elif isinstance(x.matrix, codes.ParameterCoeff) and x.matrix.cols == 1:
            if ijv == "v":
                val = x.op % ("%s[i]" % matrix)
            return "for(i = 0; i < {length}; ++i) *%(ptr)s++ = {value};".format(length=x.matrix.rows, value = val)
        else:
            return "for(i = 0; i < %s->nnz; ++i) *%%(ptr)s++ = %s;" % (matrix, x.op % ("%s->%s[i]" % (matrix, ijv)))
    return to_str

def _range(x):
    if x.stride == 1:
        return "for(i = %s; i < %s; ++i) *%%(ptr)s++ = i;" % (x.start, x.end)
    else:
        return "for(i = %s; i < %s; i+=%s) *%%(ptr)s++ = i;" % (x.start, x.end, x.stride)

def repeat(x):
    return "for(i = 0; i < %s; ++i) *%%(ptr)s++ = %s;" % (x.n, toC(x.obj))

def assign(x):
    raise Exception("Assignment not implemented.... %s = %s" % (x.lhs, x.rhs))

def nnz(x):
    if isinstance(x.obj, codes.ParameterCoeff) and x.obj.cols == 1:
        return "%s" % (x.obj.rows)
    return "%s->nnz" % (toC(x.obj))

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
    codes.LoopRows:                 loop("i"),
    codes.LoopCols:                 loop("j"),
    codes.LoopOver:                 loop("v"),
    codes.Range:                    _range,
    codes.Repeat:                   repeat,
    codes.Assign:                   assign,
    codes.NNZ:                      nnz,
    str:                            lambda x: x,
    int:                            lambda x: str(x),
    float:                          lambda x: str(x),
    AbstractDim:                    lambda x: str(x)
}

toC = create_encoder(lookup)
