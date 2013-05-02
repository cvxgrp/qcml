import qc_geo_mean as geo_mean
from scoop.qc_ast import increasing, decreasing, nonmonotone, \
     Positive, Negative, ispositive, isnegative, \
     Convex, Concave, Affine, Constant
from utils import annotate


""" This is the sqrt atom.

        sqrt(x) = geo_mean(x,1)
        
    It is a CONCAVE atom. It is INCREASING in the first argument.
    
    It returns a VECTOR expression.
    
    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
def attributes(x):
    return geo_mean.attributes(x, Constant(1.0))

@annotate('sqrt')
def rewrite(p,x):
    """ Rewrite a square node
        
        p
            the parent node
        
        x
            the argument
    """
    return geo_mean.rewrite(p,x,Constant(1.0))

    # v = Variable(create_varname(), shape)
    #                 
    #     # declare the expansion in "SCOOP"
    #     if isscalar(shape):
    #         definition = [
    #             v,  # declare the variable
    #             # norm([(1/2)(y-v); x]) <= (1/2)(y + v)
    #             # norm([y-v; 2x]) <= y+v
    #             Cone.SOC(y + v, [y - v, Constant(2.0)*x]),
    #             y >= Constant(0)
    #         ]
    # 
    #     else:
    #         definition = [
    #             v, # declare the variable
    #             # norm([(1/2)(y-v); x]) <= (1/2)(y + v)
    #             # norm([y-v; 2x]) <= y+v
    #             Cone.SOC(y + v, y - v, Constant(2.0)*x),
    #             y >= Constant(0)
    #         ]
    #


