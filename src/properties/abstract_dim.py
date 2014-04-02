import collections

def list_product(l):
    """ Take a list and return an AD by multiplying list elements together.
    """
    return reduce(lambda x,y: x * y, l, AbstractDim(1))


class AbstractDim(object):
    """ Implemented by holding a collections.Counter to keep track of terms.

        Counter is held by composition rather than inheriting from there; makes
        __init__ somewhat simpler and is cleaner.

        The key 1 is used for constant terms.  All other keys should be strs
        representing abstract terms.

        Designed to have reasonable arithmetic operations with plain ints.
        Didn't bother to worry about Python2 long type.  Doesn't currently deal
        with floats.

        In general plain ints are promoted to ADs in operations, but if the AD
        is concrete then it can be converted back into an int.  Currently this
        is done in most cases, although perhaps would be cleaner not to be so
        generous.

        I didn't implement all possible arthmetic or coercions, only the ones
        that were needed to get codegen to run on example problems.  So there
        may be things missing for general case.
    """
    def __init__(self, *args, **kwargs):
        self._c = collections.Counter(kwargs)
        for a in args:
            if   isinstance(a, dict): self._c.update(a)
            elif isinstance(a, int):  self._c.update({1:a})
            elif isinstance(a, str):  self._c.update([a])
            else:                     self._c.update(a)

    @property
    def concrete(self):
        """ AD is concrete means it can be converted into an int, i.e. it has
            no abstract part with nonzero coefficients.
        """
        nzkeys = self.nzkeys()
        if len(nzkeys) == 0: return True
        return len(nzkeys) == 1 and self._c[1] != 0

    def keys(self):
        return self._c.keys()

    def nzkeys(self):
        """ List of keys with nonzero values (i.e. coefficients)
        """
        return filter(lambda x: self._c[x] != 0, self.keys())

    def __str__(self):
        """ Protect with parentheses if more than one term.  Don't bother to
            print terms with coefficient == 0
        """
        printkeys = self.nzkeys()
        if not printkeys: return "0"
        ret = ' + '.join(map(self._str_term, sorted(printkeys)))
        if len(printkeys) < 2: return ret
        return "(%s)" % ret

    def _str_term(self, key):
        """ Format single term nicely
        """
        if not key in self._c: return ''
        if key == 1:           return str(self._c[1])
        if self._c[key] == 1:  return key
        return "%d*%s" % (self._c[key], key)

    def __int__(self):
        if self.concrete: return self._c[1]
        return None

    def __float__(self):
        if self.concrete: return float(self._c[1])
        return None

    def __eq__(self, other):
        """ Coefficient operations like codegen_mul check whether expressions
            == 1 or == -1 to allow simplifications.  So we want to be able to
            have AbstractDim(1) == 1 -> True
        """
        if not isinstance(other, (AbstractDim, int)):
            return NotImplemented
        if isinstance(other, AbstractDim):
            return self._c == other._c
        if isinstance(other, int) and self.concrete:
            return int(self) == other
        return False

    def __mul__(self, other):
        if not isinstance(other, (AbstractDim, int)):
            return NotImplemented

        if isinstance(other, int):
            if self.concrete: return int(self) * other
            return self * AbstractDim(other)

        if self.concrete:
            mul = AbstractDim()
            for k,v in other._c.iteritems(): mul._c[k] = int(self)*v
            return mul
        if other.concrete:
            mul = AbstractDim()
            for k,v in self._c.iteritems(): mul._c[k] = other._c[1]*v
            return mul
        ops = sorted([str(self), str(other)])
        mulkey = "%s * %s" % (ops[0], ops[1])
        return AbstractDim(mulkey)

    def __div__(self, other):
        if not isinstance(other, (AbstractDim, int)):
            return NotImplemented

        if isinstance(other, int):
            if self.concrete:
                return int(self) / other
            return self / AbstractDim(other)

        if self.concrete:
            div = AbstractDim()
            for k,v in other._c.iteritems(): div._c[k] = int(self)/v
            return div
        if other.concrete:
            div = AbstractDim()
            for k,v in self._c.iteritems(): div._c[k] = v/other._c[1]
            return div
        divkey = "%s / %s" % (self, other)
        return AbstractDim(divkey)


    def __add__(self, other):
        if not isinstance(other, (AbstractDim, int)):
            return NotImplemented

        if isinstance(other, int):
            if self.concrete:
                return int(self) + other
            return self + AbstractDim(other)

        return AbstractDim(self._c + other._c)

    def __sub__(self, other):
        if not isinstance(other, (AbstractDim, int)):
            return NotImplemented

        if isinstance(other, int):
            if self.concrete:
                return int(self) - other
            return self - AbstractDim(other)

        sub = self._c.copy()
        sub.subtract(other._c)
        return AbstractDim(sub)

    def __radd__(self, other):
        return self + other

    def __rmul__(self, other):
        return self * other

if __name__ == "__main__":
    print list_product([5, 'a'])
