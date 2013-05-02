import qc_geo_mean
import qc_square
import qc_sqrt
import qc_square_over_lin
from scoop.qc_ast import Variable, isconstant, Expression, Convex, Concave, increasing
from utils import create_varname, annotate

import scoop
from scoop.qc_ast import Atom, Constant

square = qc_square.rewrite
square_over_lin = qc_square_over_lin.rewrite
sqrt = qc_sqrt.rewrite
geo_mean = qc_geo_mean.rewrite

""" Identity function
"""
def id_func(p, x):
    return (x, [])

def id_attributes(x):
    return (x.sign, x.vexity, x.shape)

""" x^4
"""
def quad(p,x):
    v1, d1 = square(p,x)
    v2, d2 = square(p,v1)
    
    return (v2, d1+d2)

""" x^3
"""
def cube(p,x):
    v1, d1 = square(p,x)
    v2, d2 = square_over_lin(p,v1,x)
    
    return (v2, d1 + d2)
    
""" x^(1/4)
"""
def one_fourth(p,x):
    v1, d1 = sqrt(p,x)
    v2, d2 = sqrt(p,v1)
    
    return (v2, d1+d2)

""" x^(3/4)
"""
def three_fourths(p,x):
    v1, d1 = sqrt(p,x)
    v2, d2 = geo_mean(p,x,v1)

    return (v2, d1 + d2)

""" x^(4/3)
"""
def four_thirds(p,x):
    v = Variable(create_varname(), p.shape)
    
    var, d = three_fourths(p,v)
    d.append(x <= var)
    
    return (v, d)
    
""" x^(3/2)
"""
def three_halves(p,x):
    v1, d1 = sqrt(p,x)
    v2, d2 = square_over_lin(p,x,v1)
    
    return (v2, d1 + d2)
    
""" x^(1/3)
"""
def one_third(p,x):    
    v = Variable(create_varname(), p.shape)
    
    var, d = cube(p,v)
    d.append(x <= var)

    return (v, d)
    
    
""" x^(2/3)
"""
def two_thirds(p,x):
    v = Variable(create_varname(), p.shape)
    
    var, d = three_halves(p,v)
    d.append(x <= var)

    return (v, d)

# only valid entries for pow_rat
valid = {
    (1,1): (id_func,id_attributes),
    (1,2): (sqrt,qc_sqrt.attributes),
    (1,3): (one_third,qc_sqrt.attributes),
    (1,4): (one_fourth,qc_sqrt.attributes),
    (2,1): (square,qc_square.attributes),
    (2,2): (id_func,id_attributes),
    (2,3): (two_thirds,qc_sqrt.attributes),
    (2,4): (sqrt,qc_sqrt.attributes),
    (3,1): (cube,qc_square.attributes),
    (3,2): (three_halves,qc_sqrt.attributes),
    (3,3): (id_func,id_attributes),
    (3,4): (three_fourths,qc_sqrt.attributes),
    (4,1): (quad,qc_square.attributes),
    (4,2): (square,qc_square.attributes),
    (4,3): (four_thirds,qc_square.attributes),
    (4,4): (id_func,id_attributes)
}

def attributes(x,p,q):
    if isconstant(p) and isconstant(q):
        # get the power
        rat = (p.value, q.value)
        def pow_rat_err(arg):
            raise TypeError("Nonexistent implementation for %s^(%s/%s)" % (arg.value, rat[0], rat[1]))
        
        scoop.qc_atoms.pow_func, attributes = valid.get(rat, (None, pow_rat_err))
        return attributes(x)

    else:
        raise TypeError("Cannot use non-constant arguments p = %s, q = %s" % (str(p), str(q)))

# pow_rat(x,p,q) = x.^(p/q)
@annotate('pow_rat')
def rewrite(node,x,p,q):
    return scoop.qc_atoms.pow_func(node, x)
    
       
