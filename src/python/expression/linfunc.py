from scoop.codegen import Row, Col

def display_coeff(x):
    k,v = x
    if k == '1':
        if v == 0:  # or epsilon
            return ''
        else:
            return str(v)
    elif v == 1:
        return str(k)
    elif v == -1:
        return '-' + str(k)
    elif v == 0:
        return ''
    else:
        return "%s*%s" % (str(v), str(k))

def display_linear_func(x): 
    k,v = x
    
    if k == '1':
        if v.iszero():  # or epsilon
            return ''
        else:
            return str(v)
    elif v.isone():
        return str(k)
    elif v.isneg():
        return '-' + str(k)
    elif v.iszero():
        return ''
    else:
        coeffs = str(v).split(' + ')
        term = map(lambda e: "%s*%s" % (e, str(k)), coeffs)
        return ' + '.join(term)

def filter_zero(d):
    return dict( (k,v) for k,v in d.iteritems() if v != 0.0 )
def linfunc_filter_zero(d):
    return dict( (k,v) for k,v in d.iteritems() if not ('1' in v.coeff_dict and v.coeff_dict['1'] == 0.0) )

class Coeff(object):
    """A class / container for storing coefficients. These are stored in the 
    form of c + a1*p1 + a2*p2 + ... , where pi are parameters.
    
    This assumes that parameters are uniquely identified by their names. That
    is, we can't have a parameter named 'x' that is a positive scalar and
    another parametered named 'x' that is suddently a negative matrix.
    """
    def __init__(self, size, dictionary={}):
        if dictionary: self.coeff_dict = dictionary
        else: self.coeff_dict = {'1': 0}
        self.rows, self.cols = size
    
    def __repr__(self):
        return "Coeff((%s,%s), %s)" % (self.rows, self.cols, self.coeff_dict)
    
    def __str__(self):
        # sort the dictionary before displaying it
        d = sorted(self.coeff_dict.items(), key=lambda t:t[0], reverse=True)
        
        s = ' + '.join( filter(None, map(display_coeff, d)) )
        if s: return s
        else: return "0"
            
    # Coeff creation happens though class methods
    @classmethod
    def constant(self, val):
        return Coeff((Row(1),Col(1)), {'1': val})
        
    @classmethod
    def parameter(self, s):
        if isinstance(s,str):
            return Coeff((Row(s), Col(s)), {s: 1})
        else:
            raise Exception("Cannot create a coefficient")
            
    def isconstant(self):
        return ('1' in self.coeff_dict and len(self.coeff_dict) == 1)
    
    def isone(self):
        """True if coeff represents 1.0"""
        return self.isconstant() and self.coeff_dict['1'] == 1.0

    def isneg(self):
        """True if coeff represents -1.0"""
        return self.isconstant() and self.coeff_dict['1'] == -1.0
        
    def iszero(self):
        """True if coeff represents 0"""
        return (self.isconstant() and self.coeff_dict['1'] == 0.0)
    
    
    def __add__(self, other):
        # add the constants
        a = self.coeff_dict
        b = other.coeff_dict
        # got this nice piece of code off stackoverflow http://stackoverflow.com/questions/1031199/adding-dictionaries-in-python
        d = dict( (n, a.get(n, 0) + b.get(n, 0)) for n in set(a)|set(b) )
        dims = (self.rows + other.rows, self.cols + other.cols)
        
        return Coeff(dims, filter_zero(d))
    
    def __neg__(self):
        for k in self.coeff_dict:
            self.coeff_dict[k] = -self.coeff_dict[k]
        return self
    
    def __sub__(self,other):
        # subtract the constants
        a = self.coeff_dict
        b = other.coeff_dict
        d = dict( (n, a.get(n, 0) - b.get(n, 0)) for n in set(a)|set(b) )
        dims = (self.rows + other.rows, self.cols + other.cols)
        
        return Coeff(dims, filter_zero(d))
    
    def __mul__(self,other):
        # multiplying coefficients
        # creates new keys
        # (c1 + a1*p1)*(c2 + a2*p2) = (c1*c1) + (c1*a2)*p2 + (c2*a1)*p1 + (a1*a2)*(p1*p2)
        d = {}
        for k1,v1 in self.coeff_dict.items():
            for k2,v2 in other.coeff_dict.items():
                if k1 == '1' and k2 == '1':
                    d['1'] = v1*v2
                elif k1 == '1':
                    v = d.get(k2, 0)
                    d[k2] = v + v1*v2
                elif k2 == '1':
                    v = d.get(k1, 0)
                    d[k1] = v + v2*v1
                else:
                    d[k1 + '*' + k2] = v1*v2
        if other.cols.size == 1:
            # usually, "1" is a placeholder for a diagonal matrix
            dims = (self.rows, self.cols)
        else:
            dims = (self.rows, other.cols)
        
        return Coeff(dims, filter_zero(d))
        # (c + a1*p1)*c 

ZERO = Coeff.constant(0)   

class LinearFunc(object):
    """A class that stores linear functions as a dictionary of coefficients
    and variable names. Any expression of the form a*x + b*y + c can be stored
    in this object.
    """
    
    def __init__(self, dictionary={}):
        if dictionary: self.linear_dict = dictionary   # function represented as a dictionary
        else: self.linear_dict = {'1': ZERO}
        

    def __repr__(self):
        return "LinearFunc(%s)" % self.linear_dict
    
    def __str__(self):
        # sort the dictionary before displaying it
        d = sorted(self.linear_dict.items(), key=lambda t:t[0], reverse=True)
        s = ' + '.join( filter(None, map(display_linear_func, d)) )
        if s: return s
        else: return "0"
    
    def has_constant_coeff(self):
        return ('1' in self.linear_dict and len(self.linear_dict) == 1)
    
    def isconstant(self):
        return ('1' in self.linear_dict and
             len(self.linear_dict) == 1 and
              self.linear_dict['1'].isconstant())
    
    def constant_value(self):
        if self.isconstant(): return self.linear_dict['1'].coeff_dict['1']
        else: return None
    

        
            
    # def iszero(self):
    #     """True if linear func represents 0"""
    #     # only true if the function is constant and the coeff is zero
    #     return self.isconstant() and self.linear_dict['1'].iszero()
            
        
    # linear function creation happens through class methods
    @classmethod
    def constant(self, val):
        if isinstance(val,str):
            return LinearFunc({'1': Coeff.parameter(val)})
        elif isinstance(val,int) or isinstance(val,float):
            return LinearFunc({'1': Coeff.constant(val)})
        elif isinstance(val,Coeff):
            return LinearFunc({'1': val})
        else:
            raise Exception("Cannot create a constant linear function from %s" % val)
        
    @classmethod
    def variable(self, s):
        if isinstance(s, str):
            return LinearFunc({s: Coeff.constant(1.0)})
        else:
            raise Exception("Cannot create a linear function from %s" % val)
    
    def get_dimensions(self):
        rows = Row(1)
        for v in self.linear_dict.values():
            rows += v.rows
        
        dims = dict( (k, (rows, v.cols)) for k, v in self.linear_dict.iteritems() )

        return dims
    
    def __add__(self, other):
        # add the constants
        a = self.linear_dict
        b = other.linear_dict
        # got this nice piece of code off stackoverflow http://stackoverflow.com/questions/1031199/adding-dictionaries-in-python
        d = dict( (n, a.get(n, ZERO) + b.get(n, ZERO)) for n in set(a)|set(b) )
        
        return LinearFunc(linfunc_filter_zero(d))
    
    def __neg__(self):
        for k in self.linear_dict:
            self.linear_dict[k] = -self.linear_dict[k]
        return self
    
    def __sub__(self,other):
        # subtract the constants
        a = self.linear_dict
        b = other.linear_dict
        d = dict( (n, a.get(n, ZERO) - b.get(n, ZERO)) for n in set(a)|set(b) )

        return LinearFunc(linfunc_filter_zero(d))
    
    def __mul__(self,other):
        # only allow f*g when f is a constant, only left-hand mults are 
        # allowed. d*(a*x + b*y + c) = (d*a)*x + (d*b)*y + d*c
        #        
        # don't even bother checking if RHS is a constant
        if self.has_constant_coeff():
            d = {}
            vc = self.linear_dict['1']
            for k,v in other.linear_dict.items():
                d[k] = vc * v
                
            return LinearFunc(linfunc_filter_zero(d))
        else:
            raise Exception("Cannot multiply two linear functionals. Nonconvex operation.")
    
    
    