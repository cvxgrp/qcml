from qcml.expressions.expression import Number, Parameter, Variable, Transpose, Sum
from qcml.properties.shape import Scalar, Matrix, Vector
from qcml.properties.sign import Positive, Neither
from qcml.properties.curvature import isconvex, isconcave, isaffine, isnonconvex, isconstant
import qcml.atoms.atom as atom

a = Number(1)
b = Number(2)
c = Number(3)
x = Variable('x', Scalar())
y = Variable('y', Scalar())
z = Variable('z', Vector('n'))
w = Parameter('w', Scalar(), Positive())
u = Parameter('u', Scalar(), Neither())
D = Parameter('D', Matrix('m', 'p'), Neither())
E = Parameter('E', Matrix('p', 'n'), Neither())

constants = [a, b, a + b]
affine = [a, b, c, a + b, a + w, w + u, w*x, x + w, w - y, D*E*z, Sum(z)]
# TODO: add more test cases to convex, concave
convex = [  # x^2
            atom.QC_square(x), 
            # c*x + w*x^2
            c*x + w*atom.QC_square(x), 
             # ||x||
            atom.QC_norm(x),
            # ||x||_2^2
            atom.QC_square(atom.QC_norm(x)), 
            # (x-y)^2/(1 - max(x,y))
            atom.QC_quad_over_lin(x - y, a - atom.QC_max(x, y)),
            # square(D*E*z) - x^(3/4)
            atom.QC_square(D*E*z) - atom.QC_pow_rat(x, Number(3), Number(4)),
            # x^(-2/3)
            atom.QC_inv_pos(atom.QC_pow_rat(x, Number(2), Number(3))),
            # x^(-4/3)
            atom.QC_square(atom.QC_inv_pos(atom.QC_pow_rat(x, Number(2), Number(3)))) ]
# TODO: add more nonconvex test cases
nonconvex = [   atom.QC_sqrt(a + atom.QC_square(x)), 
                c*x + u*atom.QC_square(x),
                atom.QC_square(D*E*z) + atom.QC_pow_rat(x, Number(3), Number(4)) ]

def check(is_curvature, x):
    return is_curvature(x)
    
def test_vexities():
    for expr in constants:
        yield isconstant, expr
        yield isaffine, expr
        yield isconvex, expr
        yield isconcave, expr
    
    for expr in affine:
        yield isaffine, expr
        yield isconvex, expr
        yield isconcave, expr
    
    for expr in convex:
        yield isconvex, expr
    
    for expr in nonconvex:
        yield isnonconvex, expr
        
        