
def ispositive(x):
    return x.sign.value > 0

def isnegative(x):
    return x.sign.value < 0

# @staticmethod
# def isneither(x):
#     return isinstance(x.sign, Neither)

class Sign(object):
    def __init__(self, value=0):
        # uses math to keep track of the sign
        if value == 0: self.value = 0   # neither
        elif value > 0: self.value = 1  # positive
        else: self.value = -1           # negative

    def __add__(self,other):
        val = self.value + other.value
        if val == 2: return Positive()
        if val == -2: return Negative()
        return Neither()

    def __sub__(self,other):
        val = self.value - other.value
        if val == 2: return Positive()
        if val == -2: return Negative()
        return Neither()

    def __mul__(self,other):
        return Sign(self.value * other.value)

    def __neg__(self):
        return Sign(-self.value)

    def __str__(self):
        if self.value ==0: return "neither"
        if self.value > 0: return "positive"
        if self.value < 0: return "negative"

""" Convenience functions for creating signs. Uppercase to "fake" class
    creation.
"""
def Positive(): return Sign(1)

def Negative(): return Sign(-1)

def Neither(): return Sign(0)