
""" Dimension, a class for manipulating (simple) abstract dimension algebra.
"""
class Dimension(object):
    lookup = None
    
    def __init__(self,value,data={}):
        self.data = data
        if isinstance(value, str):
            try:
                value = int(value)
                self.data = {'1': value}
            except ValueError:
                self.data = {'1': 0, value: 1}
        if isinstance(value, float) or isinstance(value, int):
            self.data = {'1' : int(value)}
    
    def eval(self):
        if self.lookup:
            # evaluate the dimension object
            for k,v in self.data.iteritems():
                if k != '1' and self.lookup.get(k,None) is not None:
                    self.data['1'] += self.lookup[k]*v
                    self.data[k] = 0
        
    
    def __add__(self, other):
        left = self.data
        right = other.data
        result = dict(left)
        
        for k in right.keys():
            result[k] = left.get(k,0) + right[k]
        
        return Dimension(None, result)
    
    def __str__(self):
        self.eval()

        const = self.data.get('1',0)
        # prints coeffs > 1 (ignores 0 coeff)
        terms = ["%s*%s" % (v,k) for k,v in self.data.iteritems() if k != '1' and v > 1]
        # prints coeff == 1
        terms += ["%s" % (k) for k,v in self.data.iteritems() if k != '1' and v == 1]
        if const != 0:
            terms.append(str(const))
            return ' + '.join(terms)
        elif terms:
            return ' + '.join(terms)
        else:
            return str(const)