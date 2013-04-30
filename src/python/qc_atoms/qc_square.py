import qc_quad_over_lin as qol
from scoop.qc_ast import increasing, decreasing, nonmonotone, \
     Positive, Negative, ispositive, isnegative, \
     Convex, Concave, Affine, Constant


""" This is the square atom.

        square(x) = quad_over_lin(x,1)
    
    If y is a vector, it computes the division elementwise.
    
    It is a CONVEX atom. It is NONMONOTONE in the first argument.
    
    If the first argument is POSITIVE, it is INCREASING in the first argument.
    If the first argument is NEGATIVE, it is DECRASING in the first argument.
    
    It returns a VECTOR expression.
    
    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program
"""
def attributes(x):
    return qol.attributes(x, Constant(1.0))

def rewrite(p,x):
    """ Rewrite a square node
        
        p
            the parent node
        
        x
            the argument
    """
    return qol.rewrite(p,x,Constant(1.0))

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


