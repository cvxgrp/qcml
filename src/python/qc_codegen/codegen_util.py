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
"""
class CodegenExpr(object):
    # operations only occur on objects with the same shape
    def __add__(self, other):
        if isinstance(self, Constant) and isinstance(other, Constant):
            return Constant(self.value + other.value)
        if isinstance(self, Constant) and self.value == 0:
            return other
        if isinstance(other,Constant) and other.value == 0:
            return self
        if isinstance(self,Eye) and isinstance(other,Eye):
            return Eye(self.n, self.coeff + other.coeff)
        if isinstance(self,Ones) and isinstance(other,Ones) and (self.transpose == other.transpose):
            return Ones(self.n, self.coeff + other.coeff, self.transpose)
        if str(self) == str(other):
            return Constant(2.0) * self
        return Add(self, other)

    def __sub__(self, other):
        return self + (-other)

    def __neg__(self):
        if isinstance(self,Negate):
            return self.arg
        if isinstance(self,Transpose):
            return Transpose(-self.arg)
        if isinstance(self, Constant):
            return Constant(-self.value)
        if isinstance(self,Eye):
            return Eye(self.n, -self.coeff)
        if isinstance(self,Ones):
            return Ones(self.n, -self.coeff)
        if isinstance(self,Mul):
            return Mul(-self.left, self.right)
        return Negate(self)

    def __mul__(self,other):
        if isinstance(self, Constant) and isinstance(other, Constant):
            return Constant(self.value * other.value)
        if isinstance(self,Constant) and self.value == 1:
            return other
        if isinstance(other,Constant) and other.value == 1:
            return self

        if isinstance(self,Eye) and other.isknown and other.isscalar:
            return Eye(self.n, self.coeff * other)
        if isinstance(other,Eye) and self.isknown and self.isscalar:
            return Eye(other.n, other.coeff * self)

        if isinstance(self,Eye) and isinstance(other,Eye):
            # (a*I) * (b*I) = (a*b*I)
            return Eye(self.n, self.coeff * other.coeff)
        if isinstance(self,Eye) and isinstance(self.coeff, Constant) and self.coeff.value == 1:
            # I*x = x
            return other
        if isinstance(other,Eye) and isinstance(other.coeff, Constant) and other.coeff.value == 1:
            # x*I = x
            return self
        if isinstance(self,Eye) and isinstance(self.coeff, Constant) and self.coeff.value == -1:
            return -other
        if isinstance(other,Eye) and isinstance(other.coeff, Constant) and other.coeff.value == -1:
            return -self
        if isinstance(self,Ones) and self.transpose and isinstance(other,Ones) and not other.transpose:
            # ones^T ones
            return Parameter(self.n) * self.coeff * other.coeff
        if isinstance(self,Ones) and other.isknown and other.isscalar:
            return Ones(self.n, self.coeff*other, self.transpose)
        if isinstance(other,Ones) and self.isknown and self.isscalar:
            return Ones(other.n, other.coeff*self, other.transpose)

        return Mul(self, other)

    def trans(self):
        if self.isscalar:
            return self
        if isinstance(self, Eye):
            return self
        if isinstance(self, Ones):
            self.transpose = not self.transpose
            return self
        if isinstance(self,Transpose):
            return self.arg
        if isinstance(self, Slice):
            self.transpose = not self.transpose
            return self

        return Transpose(self)

    # only used during code generation
    # for vertical slicing
    def slice(self, begin, end):
        if self.isscalar:
            return self

        if isinstance(self,Ones):
            if not self.transpose:
                self.n = end - begin
            return self
        if isinstance(self,Negate):
            return Negate(self.arg.slice(begin, end))
        if isinstance(self,Add):
            return Add(self.left.slice(begin,end), self.right.slice(begin,end))
        if isinstance(self,Mul):
            return Mul(self.left.slice(begin,end), self.right)
        if isinstance(self,Transpose):
            return Slice(self.arg, begin, end, transpose=True)
        if isinstance(self, Slice):
            # a(2:5)(1:2)
            return Slice(self.arg, self.begin + begin, self.begin + end)

        return Slice(self, begin, end)

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
