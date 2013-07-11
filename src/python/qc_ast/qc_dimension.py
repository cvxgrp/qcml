
""" Dimension, a class for manipulating (simple) abstract dimension/integer algebra.
"""
class Dimension(object):
    lookup = None
    
    def __init__(self,value,data={}):
        self.__data = data
        if isinstance(value, str):
            try:
                value = int(value)
                self.__data = {'1': value}
            except ValueError:
                # WARNING: if the string is totally bogus, this will still
                # create a Dimension object for it, with the bogus string as
                # the key
                self.__data = {value: 1}
        if isinstance(value, float) or isinstance(value, int):
            self.__data = {'1' : int(value)}
    
    def eval(self):
        # modifies the Dimension object in place
        if self.lookup:
            # evaluate the dimension object
            const, new_dict = 0, {}
            for k,v in self.__data.iteritems():
                if k != '1' and self.lookup.get(k,None) is not None:
                    const += self.lookup[k]*v
                else:
                    new_dict[k] = v
            if const > 0: new_dict['1'] = const 
            self.__data = new_dict
        
    
    def __add__(self, other):
        # allow Dimension + int and Dimension + Dimension
        if isinstance(other, int):
            right = {'1': other}
        else:
            right = other.__data
            
        left = self.__data
        result = dict(left)
        
        for k in right.keys():
            result[k] = left.get(k,0) + right[k]
        
        return Dimension(None, result)
    
    def __eq__(self,other):
        if isinstance(other, Dimension):
            if self.lookup:
                self.eval()
                other.eval()
            return self.__data == other.__data
        if isinstance(other, int):
            if self.lookup: self.eval()
            return self.__data == {'1': other}
        if isinstance(other, str):
            return str(self) == other
        raise SyntaxError("Cannot compare %s to %s\n" % (type(self), type(other)))
        
    def __ne__(self,other):
        return not self.__eq__(other)
    
    def __str__(self):
        self.eval()
        const = self.__data.get('1',None)
        # prints coeffs > 1 (ignores 0 coeff)
        terms = ["%s*%s" % (v,k) for k,v in self.__data.iteritems() if k != '1' and v > 1]
        # prints coeff == 1
        terms.extend(["%s" % (k) for k,v in self.__data.iteritems() if k != '1' and v == 1])
        # adds constant
        if const is not None: terms.append(str(const))
        return ' + '.join(terms)
    
    def __repr__(self):
        return "Dimension(None, %s)" % self.__data