# define code generation objects
# TODO: can change behavior of these by changing __str__ definition
""" CodegenExpr object.
    Used to construct coefficients of affine expressions.

    Allows us to create abstract objects to represent Ones, Eye, etc. and
    perform basic addition, multiplication, etc. on them. Assumes that the
    coefficient math has already passed DCP checking.

    The behavior can be changed by redefining __str__ definition. These classes
    are *final*. You should not subclass them (undefined behavior will occur).

    Note that by setting the __str__ function, e.g., Ones.__str__ = some_func,
    it is a *global* change to all Ones objects.

    To allow interchangeability with different codegens, the __str__ function
    is set in the __init__() function of subclasses of the Codegen object.

    However, this means that *two* Codegen objects cannot exist simultaneously,
    as the __str__ function on, say, Ones, is overwritten by the latest
    Codegen object.

    This is not an issue as long as code generators are only created to generate
    the code and not stored for later use.

    TODO: Possibly figure out how to redefine __str__ without this happening.
    TODO: Or, better, figure out how to modify operators to return the appropriate
        subclass.

    Neither approaches above seem very likely.

    TODO: Add test cases to CodegenExpr
"""
class CodegenExpr(object):
    # operations only occur on objects with the same shape
    def __add__(self, other): return codegen_add(self, other)

    def __sub__(self, other): return self + (-other)

    def __neg__(self): return codegen_negate(self)

    def __mul__(self,other): return codegen_mul(self, other)

    def trans(self): return codegen_transpose(self)

    # only used during code generation
    # for vertical slicing
    def slice(self, begin, end): return codegen_slice(self, begin, end)

    def __repr__(self): return str(self)

class Constant(CodegenExpr):
    def __init__(self, value):
        self.value = value
        self.isknown = True
        self.isscalar = True

    def __str__(self): return str(self.value)

class Parameter(CodegenExpr):
    def __init__(self, value):
        self.value = value
        self.isknown = True
        self.isscalar = False

    def __str__(self): return self.value

class ScalarParameter(Parameter):
    def __init__(self,value):
        super(ScalarParameter, self).__init__(value)
        self.isscalar = True

class Negate(CodegenExpr):
    def __init__(self, arg):
        self.arg = arg
        self.isknown = arg.isknown
        self.isscalar = arg.isscalar

    def __str__(self): return "-(%s)" % self.arg

class Eye(CodegenExpr):
    def __init__(self, n, coeff):
        self.n = n
        self.coeff = coeff
        self.isknown = True
        self.isscalar = False

    # def __str__(self): return "_o.spmatrix(%s,range(%s),range(%s), tc='d')" % (self.coeff, self.n, self.n)

class Ones(CodegenExpr):
    def __init__(self, n, coeff, transpose = False):
        self.n = n
        self.coeff = coeff
        self.transpose = transpose
        self.isknown = True
        self.isscalar = False

    # def __str__(self):
    #     if self.transpose:
    #         return "_o.matrix(%s,(1,%s), tc='d')" % (self.coeff, self.n)
    #     else:
    #         return "_o.matrix(%s,(%s,1), tc='d')" % (self.coeff, self.n)

class Add(CodegenExpr):
    def __init__(self, left, right):
        self.left = left
        self.right = right
        self.isknown = left.isknown and right.isknown
        self.isscalar = left.isscalar and right.isscalar

    def __str__(self): return "%s + %s" % (self.left, self.right)

class Mul(CodegenExpr):
    def __init__(self, left, right):
        self.left = left
        self.right = right
        self.isknown = left.isknown and right.isknown
        self.isscalar = left.isscalar and right.isscalar

    def __str__(self): return "%s * %s" % (self.left, self.right)


class Transpose(CodegenExpr):
    def __init__(self, arg):
        self.arg = arg
        self.isknown = arg.isknown
        self.isscalar = arg.isscalar

    # def __str__(self): return "(%s).trans()" % self.arg

class Slice(CodegenExpr):
    def __init__(self, arg, begin, end, transpose=False):
        self.arg = arg
        self.begin = begin
        self.end = end
        self.transpose = transpose
        self.isknown = arg.isknown
        self.isscalar = arg.isscalar

    # def __str__(self): return "(%s)[%s:%s]" % (self.arg, self.begin, self.end)

""" Helper functions for expr ops.

    Code structure documents the simplifications that occur.
"""
def codegen_add(x,y):
    if isinstance(x, Constant) and isinstance(y, Constant):
        return Constant(x.value + y.value)
    if isinstance(x, Constant) and x.value == 0:
        return y
    if isinstance(y,Constant) and y.value == 0:
        return x
    if isinstance(x,Eye) and isinstance(y,Eye):
        return Eye(x.n, x.coeff + y.coeff)
    if isinstance(x,Ones) and isinstance(y,Ones) and (x.transpose == y.transpose):
        return Ones(x.n, x.coeff + y.coeff, x.transpose)
    if str(x) == str(y):
        return Constant(2.0) * x
    return Add(x, y)

def codegen_negate(x):
    if isinstance(x,Negate):
        return x.arg
    if isinstance(x,Transpose):
        return Transpose(-x.arg)
    if isinstance(x, Constant):
        return Constant(-x.value)
    if isinstance(x,Eye):
        return Eye(x.n, -x.coeff)
    if isinstance(x,Ones):
        return Ones(x.n, -x.coeff)
    if isinstance(x,Mul):
        return Mul(-x.left, x.right)
    return Negate(x)

def codegen_mul(x,y):
    if isinstance(x, Constant) and isinstance(y, Constant):
        return Constant(x.value * y.value)
    if isinstance(x,Constant) and x.value == 1:
        return y
    if isinstance(y,Constant) and y.value == 1:
        return x

    if isinstance(x,Eye) and y.isknown and y.isscalar:
        return Eye(x.n, x.coeff * y)
    if isinstance(y,Eye) and x.isknown and x.isscalar:
        return Eye(y.n, y.coeff * x)

    if isinstance(x,Eye) and isinstance(y,Eye):
        # (a*I) * (b*I) = (a*b*I)
        return Eye(x.n, x.coeff * y.coeff)
    if isinstance(x,Eye) and isinstance(x.coeff, Constant) and x.coeff.value == 1:
        # I*x = x
        return y
    if isinstance(y,Eye) and isinstance(y.coeff, Constant) and y.coeff.value == 1:
        # x*I = x
        return x
    if isinstance(x,Eye) and isinstance(x.coeff, Constant) and x.coeff.value == -1:
        return -y
    if isinstance(y,Eye) and isinstance(y.coeff, Constant) and y.coeff.value == -1:
        return -x
    if isinstance(x,Ones) and x.transpose and isinstance(y,Ones) and not y.transpose:
        # ones^T ones
        return Parameter(x.n) * x.coeff * y.coeff
    if isinstance(x,Ones) and y.isknown and y.isscalar:
        return Ones(x.n, x.coeff*y, x.transpose)
    if isinstance(y,Ones) and x.isknown and x.isscalar:
        return Ones(y.n, y.coeff*x, y.transpose)

    return Mul(x, y)

def codegen_transpose(x):
    if x.isscalar:
        return x
    if isinstance(x, Eye):
        return x
    if isinstance(x, Ones):
        x.transpose = not x.transpose
        return x
    if isinstance(x,Transpose):
        return x.arg
    if isinstance(x, Slice):
        x.transpose = not x.transpose
        return x

    return Transpose(x)


def codegen_slice(x, begin, end):
    if x.isscalar:
        return x

    if isinstance(x,Ones):
        if not x.transpose:
            x.n = end - begin
        return x
    if isinstance(x,Negate):
        return Negate(x.arg.slice(begin, end))
    if isinstance(x,Add):
        return Add(x.left.slice(begin,end), x.right.slice(begin,end))
    if isinstance(x,Mul):
        return Mul(x.left.slice(begin,end), x.right)
    if isinstance(x,Transpose):
        return Slice(x.arg, begin, end, transpose=True)
    if isinstance(x, Slice):
        # a(2:5)(1:2)
        return Slice(x.arg, x.begin + begin, x.begin + end)

    return Slice(x, begin, end)