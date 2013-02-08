class Sign(object):
    signs = set(['POSITIVE', 'NEGATIVE', 'UNKNOWN'])
    
    add_lookup = {
        ('POSITIVE','POSITIVE'): 'POSITIVE',
        ('NEGATIVE','NEGATIVE'): 'NEGATIVE'
    }
    
    mul_lookup = {
        ('NEGATIVE','NEGATIVE'): 'POSITIVE',
        ('POSITIVE','POSITIVE'): 'POSITIVE',
        ('NEGATIVE','POSITIVE'): 'NEGATIVE',
        ('POSITIVE','NEGATIVE'): 'NEGATIVE'
    }
    
    negate = {'POSITIVE': 'NEGATIVE', 'NEGATIVE':'POSITIVE', 'UNKNOWN':'UNKNOWN'}
    
    def __init__(self,sign_str):
        if sign_str in self.signs:
            self.sign_str = sign_str
        else:
            raise Exception("No such sign %s exists." % str(sign_str))
        
    def __repr__(self):
        return "Sign('%s')" % self.sign_str
    
    def __str__(self):
        return self.sign_str
        
    def __add__(self, other):
        lhs = self.sign_str
        rhs = other.sign_str
        
        sign = self.add_lookup.get( (lhs, rhs), 'UNKNOWN' )
        return Sign(sign)
    
    __sub__ = None
    # def __sub__(self, other):
    #     lhs = self.sign_str
    #     rhs = self.negate[other.sign_str]
    #     
    #     sign = self.add_lookup.get( (lhs, rhs), 'UNKNOWN' )
    #     return Sign(sign)
       
    def __mul__(self, other):
        lhs = self.sign_str
        rhs = other.sign_str
        
        sign = self.mul_lookup.get( (lhs, rhs), 'UNKNOWN' )
        return Sign(sign)
        
    def __neg__(self):
        self.sign_str = self.negate[self.sign_str]
        return self
        
    def __eq__(self,other):
        return self.sign_str == other.sign_str
    
    def __ne__(self,other):
        return self.sign_str != other.sign_str
        
# these are (mutable) globals, so be careful!
POSITIVE = Sign('POSITIVE')
NEGATIVE = Sign('NEGATIVE')
UNKNOWN = Sign('UNKNOWN')