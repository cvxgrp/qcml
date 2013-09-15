import collections

def list_product(l):
    return reduce(lambda x,y: x * AbstractDim(y), l, AbstractDim(1))

class AbstractDim(object):
    def __init__(self, *args, **kwargs):
        self._c = collections.Counter(kwargs)
        for a in args:
            if   isinstance(a, dict): self._c.update(a)
            elif isinstance(a, int):  self._c.update({1:a})
            elif isinstance(a, str):  self._c.update([a])
            else:                     self._c.update(a)

    @property
    def concrete(self):
        return len(self._c) == 1 and 1 in self._c
    
    def keys(self):
        return self._c.keys()

    def __str__(self):
        """ Protect with parentheses if more than one term.  Don't bother to
            print terms with coefficient == 0
        """
        printkeys = filter(lambda x: self._c[x] != 0, self.keys())
        if not printkeys: return "0"
        ret = ' + '.join(map(self._str_term, sorted(printkeys)))
        if len(printkeys) < 2: return ret
        return "(%s)" % ret

    def _str_term(self, key):
        if not key in self._c: return ''
        if key == 1:           return str(self._c[1])
        if self._c[key] == 1:  return key
        return "%d*%s" % (self._c[key], key)

    def __eq__(self, other):
        """ Coefficient operations like codegen_mul check whether expressions ==            1 or == -1 to allow simplifications.  So we want to be able to have
            AbstractDim(1) == 1 -> True
        """
        if isinstance(other, AbstractDim):
            return self._c == other._c
        if isinstance(other, int) and self.concrete:
            return self._c[1] == other
        return False

    def __mul__(self, other):
        """ Currently assumes other is an AbstractDim
        """
        if self.concrete:
            mul = AbstractDim()
            for k,v in other._c.iteritems(): mul._c[k] = self._c[1]*v 
            return mul
        if other.concrete:
            mul = AbstractDim()
            for k,v in self._c.iteritems(): mul._c[k] = other._c[1]*v
            return mul
        ops = sorted([str(self), str(other)])
        mulkey = "%s * %s" % (ops[0], ops[1])
        return AbstractDim(mulkey)

    def __div__(self, other):
        if isinstance(other, int):
            if self.concrete:
                return self._c[1] / other
            return self / AbstractDim(other)
        
        if self.concrete:
            div = AbstractDim()
            for k,v in other._c.iteritems(): div._c[k] = self._c[1]/v
            return div
        if other.concrete:
            div = AbstractDim()
            for k,v in self._c.iteritems(): div._c[k] = v/other._c[1]
            return div
        divkey = "%s / %s" % (self, other)
        return AbstractDim(divkey)


    def __add__(self, other):
        if isinstance(other, int):
            if self.concrete:
                return self._c[1] + other
            return self + AbstractDim(other)
        return AbstractDim(self._c + other._c)

    def __sub__(self, other):
        if isinstance(other, int):
            if self.concrete:
                return self._c[1] - other
            return self - AbstractDim(other)
        sub = self._c.copy()
        sub.subtract(other._c)
        return AbstractDim(sub)

    def __radd__(self, other):
        """ Currently assumes other is int
        """
        if self.concrete:
            return other + self._c[1]
        return AbstractDim(other) + self

    def __rmul__(self, other):
        """ Currently assumes other is int
        """
        if self.concrete:
            return other * self._c[1]
        return AbstractDim(other) * self

if __name__ == "__main__":
    print AbstractDim()
