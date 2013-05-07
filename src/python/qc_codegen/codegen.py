# define code generation objects
# these are for cvxopt...
# TODO: can change behavior of these by changing __str__ definition
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
        if isinstance(self, Constant):
            return Constant(-self.value)
        if isinstance(self, Parameter):
            return Parameter("-" + self.value)
        if isinstance(self,Eye):
            return Eye(self.n, -self.coeff)
        if isinstance(self,Negate):
            return self.arg
        if isinstance(self,Ones):
            return Ones(self.n, -self.coeff)
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
        if isinstance(self, Constant):
            return self
        if isinstance(self, Eye):
            return self
        if isinstance(self, Ones):
            self.transpose = not self.transpose
            return self
            
        return Transpose(self)
    
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
        
    def __str__(self): return "_o.spmatrix(%s,range(%s),range(%s), tc='d')" % (self.coeff, self.n, self.n)

class Ones(CodegenExpr):
    def __init__(self, n, coeff, transpose = False):
        self.n = n
        self.coeff = coeff
        self.transpose = transpose
        self.isknown = True
        self.isscalar = False
        
    def __str__(self): 
        if self.transpose:
            return "_o.matrix(%s,(1,%s), tc='d')" % (self.coeff, self.n)
        else:
            return "_o.matrix(%s,(%s,1), tc='d')" % (self.coeff, self.n)

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
    
    def __str__(self): return "(%s).trans()" % self.arg
