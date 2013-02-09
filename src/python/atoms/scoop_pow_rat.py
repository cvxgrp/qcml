from scoop.expression import Expression, Constant, \
    increasing, decreasing, nonmonotone, \
    ispositive, isnegative, isconstant, \
    POSITIVE, NEGATIVE, SCALAR, VECTOR, CONVEX, CONCAVE, AFFINE
from utils import create_varname
from scoop_sqrt import sqrt
from scoop_square import square
from scoop_geo_mean import geo_mean
from scoop_qol import quad_over_lin

def id_func(x):
    return ([], x)

def quad(x):
    line1, result1 = square(x)
    line2, result2 = square(result1)
    
    return (line1 + line2, result2)

def cube(x):
    line1, result1 = square(x)
    line2, result2 = quad_over_lin(result1,x)
    
    return (line1 + line2, result2)
    
def one_fourth(x):
    line1, result1 = sqrt(x)
    line2, result2 = sqrt(result1)
    
    return (line1 + line2, result2)

def three_fourths(x):
    line1, result1 = sqrt(x)
    line2, result2 = geo_mean(x,result1)
    
    return (line1 + line2, result2)

def four_thirds(x):
    v = Expression(AFFINE, POSITIVE, x.shape, create_varname(), None)
    
    line, result = three_fourths(v)
    constraint = (x <= result)

    lines = filter(None, ["variable %s %s" % (v.name, str.lower(v.shape.shape_str))]
        + line + ["%s" % str(constraint)])
    
    # assign vexity after
    v.vexity = CONVEX | increasing(x)
    return (lines, v)

def three_halves(x):
    line1, result1 = sqrt(x)
    line2, result2 = quad_over_lin(x,result1)
    
    return (line1 + line2, result2)

def one_third(x):
    v = Expression(AFFINE, POSITIVE, x.shape, create_varname(), None)
    
    line, result = cube(v)
    constraint = (x <= result)

    lines = filter(None, ["variable %s %s" % (v.name, str.lower(v.shape.shape_str))]
        + line + ["%s" % str(constraint)])
    
    # assign vexity after
    v.vexity = CONCAVE | increasing(x)
    return (lines, v)
    
def two_thirds(x):
    v = Expression(AFFINE, POSITIVE, x.shape, create_varname(), None)
    
    line, result = three_halves(v)
    constraint = (x <= result)

    lines = filter(None, ["variable %s %s" % (v.name, str.lower(v.shape.shape_str))]
        + line + ["%s" % str(constraint)])
    
    # assign vexity after
    v.vexity = CONCAVE | increasing(x)
    return (lines, v)


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
       
