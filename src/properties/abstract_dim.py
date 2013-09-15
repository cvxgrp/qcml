import collections

def list_product(l):
    return reduce(lambda x,y: x * AbstractDim([y]), l, AbstractDim([1]))


class AbstractDim(object):
    def __init__(self, *args, **kwargs):
        self._c = collections.Counter(kwargs)
        for a in args:
            if   isinstance(a, dict): self._c.update(a)
            elif isinstance(a, int):  self._c.update({1:a})
            else:                     self._c.update(a)

    @property
    def concrete(self):
        return len(self._c) == 1 and 1 in self._c

    def __str__(self):
        return ' + '.join(map(self._str_term, sorted(self._c.keys())))

    def _safe_str(self):
        """ Protect with parentheses if needed
        """
        if len(self._c) > 1: return "(%s)" % self._c
        return str(self._c)

    def _str_term(self, key):
        if not key in self._c: return ''
        if key == 1:           return str(self._c[1])
        if self._c[key] == 1:  return key
        return "%d*%s" % (self._c[key], key)

    def __mul__(self, other):
        """ Currently assumes other is an AbstractDim
        """
        if self.concrete:
            mul = AbstractDim()
            for k,v in other.iteritems(): mul[k] = self._c[1]*v 
            return mul
        if other.concrete:
            mul = AbstractDim()
            for k,v in self.iteritems(): mul[k] = other._c[1]*v
            return mul
        ops = sorted([self._safe_str(), other._safe_str()])
        mulkey = "%s * %s" % (ops[0], ops[1])
        return AbstractDim([mulkey])

    def __add__(self, other):
        return AbstractDim(self._c + other._c)

if __name__ == "__main__":
    print AbstractDim('a', 3, a=3)
