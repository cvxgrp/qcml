from scoop.expression.shape import SCALAR

def mangle(d):
    return dict( ('_'+k,v) for k,v in d.iteritems() )

def scalar():
    return 1
    
class Dimension(object):
    def __init__(self, s):
        self.size = s
    
    def __add__(self, other):
        if self.size == 1: return other
        else: return self
    
    def __eq__(self, other):
        return self.size == other.size
        
    def __ge__(self,other):
        return self.size != 1
    
class Row(Dimension):
    def __init__(self, s):
        super(Row, self).__init__(s)
    
    def __repr__(self):
        return "Row(%s)" % self.size

class Col(Dimension):
    def __init__(self, s):
        super(Col, self).__init__(s)
    
    def __repr__(self):
        return "Col(%s)" % self.size

class Codegen(object):
    """Generic code generator object for optimization problems. Contains a
    dictionary of variables used in the optimization problem, cone constraints
    on them, and the objective function."""
    def __init__(self):
        # SOCP data structures (used for codegen)
        self.variables = {}     # these are used variables
        self.parameters = {}    # these are the used parameters
        self.cones = []
        self.obj = None
        
    def __str__(self):
        print str(self.variables)
        print str(self.cones)
        print str(self.obj)
        return ""
    
    def needed_variable_dims(self, list_of_used):
        d = set( k for k,v in self.variables.iteritems() if v.shape == SCALAR )
        return d & set(list_of_used)
        
    def get_variable_sizes(self, list_of_used):
        sizes = {}
        used = set(list_of_used)    # original variables
        vectors = set()
        equiv_list = []             # list of equivalent sets
                
        for k,v in self.variables.iteritems():
            if v.shape == SCALAR:
                sizes[k] = 1
            else:
                vectors.add(k)
        
        def references_vector(x):
            dims = x.get_dimensions()
            intersect = set(dims) & vectors
            if intersect:
                return (intersect, dims)
        
        if vectors:
            # if there are any vectors we need to infer dimensions
            dims = self.obj.linfunc.get_dimensions()
            # check the objective first
            if set(dims) & vectors:
                print used & vectors
                print dims
            
            # look at all the cones and build equivalence sets
            for c in self.cones:
                lins = c.get_all_linfuncs()
                dims = filter(None, map(references_vector, lins))
                
                if dims:
                    equiv = set()
                    for e in dims: equiv |= e[0]
                    
                    for e in equiv:
                        size = sizes.get(e, None)
                        if size: sizes[e] = size
                        else: sizes[e] = Row(e)
            
            print sizes
                # row, col = Row(1), Col(1)
                # print 72*"="
                # for k in lins:
                #     print k.get_dimensions()
        # print self.variables
        # print sizes
        