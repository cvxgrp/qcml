from scoop.expression import Variable, Constant, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, isconstant, \
    POSITIVE, NEGATIVE, SCALAR, VECTOR, CONVEX, CONCAVE, AFFINE
from utils import create_varname, comment
from scoop_sqrt import sqrt
from scoop_square import square
from scoop_geo_mean import geo_mean
from scoop_qol import quad_over_lin

def id_func(x):
    return (x, [])

def quad(x):
    v1, d1 = square(x)
    v2, d2 = square(v1)
    
    return (v2, d1+d2)

def cube(x):
    v1, d1 = square(x)
    v2, d2 = quad_over_lin(v1,x)
    
    return (v2, d1 + d2)
    
def one_fourth(x):
    v1, d1 = sqrt(x)
    v2, d2 = sqrt(v1)
    
    return (v2, d1+d2)

def three_fourths(x):
    v1, d1 = sqrt(x)
    v2, d2 = geo_mean(x,v1)

    return (v2, d1 + d2)
    
def four_thirds(x):
    v = Variable(create_varname(), x.shape)
    
    var, d = three_fourths(v)
    d.append(x <= var)
    
    # assign vexity after
    v.vexity, v.sign = CONVEX | increasing(x), POSITIVE
    return (v, d)

def three_halves(x):
    v1, d1 = sqrt(x)
    v2, d2 = quad_over_lin(x,v1)
    
    return (v2, d1 + d2)

def one_third(x):    
    v = Variable(create_varname(), x.shape)
    
    var, d = cube(v)
    d.append(x <= var)

    # assign vexity after
    v.vexity, v.sign = CONCAVE | increasing(x), POSITIVE
    return (v, d)
    
def two_thirds(x):
    v = Variable(create_varname(), x.shape)
    
    var, d = three_halves(v)
    d.append(x <= var)

    # assign vexity after
    v.vexity, v.sign = CONCAVE | increasing(x), POSITIVE
    return (v, d)


# only valid entries for pow_rat
valid = {
    (1,1): id_func,
    (1,2): sqrt,
    (1,3): one_third,
    (1,4): one_fourth,
    (2,1): square,
    (2,2): id_func,
    (2,3): two_thirds,
    (2,4): sqrt,
    (3,1): cube,
    (3,2): three_halves,
    (3,3): id_func,
    (3,4): three_fourths,
    (4,1): quad,
    (4,2): square,
    (4,3): four_thirds,
    (4,4): id_func,
}
# pow_rat(x,p,q) = x.^(p/q)
@comment
def pow_rat(x,p,q):
    if isconstant(p) and isconstant(q):
        # get the power
        rat = (p.value(), q.value())
        def pow_rat_err(arg):
            raise SyntaxError("Nonexistent implementation for (%s)^(%s/%s)" % (arg.name, rat[0], rat[1]))
            
        f = valid.get(rat, pow_rat_err)

        return f(x)
    else:
        raise SyntaxError("Cannot use arguments p = %s, q = %s" % (str(p), str(q)))
       
