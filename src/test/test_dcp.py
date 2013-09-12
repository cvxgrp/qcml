from qcml.ast.expressions import Number, Parameter, Variable, Transpose, Sum
from qcml.properties.shape import Scalar, Matrix, Vector
from qcml.properties.sign import Positive, Neither
from qcml.properties.curvature import isconvex, isconcave, isaffine, isnonconvex, isconstant
import qcml.ast.atoms as atom

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

constants = [a, b, c, a + b, a + w, w + u]
affine = [w*x, x + w, w - y, D*E*z, Sum(z)]

# TODO: add more test cases to convex
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
            # x^(-4/9)
            atom.QC_square(atom.QC_inv_pos(atom.QC_pow_rat(x, Number(2), Number(3)))) ]

# TODO: add more test cases to concave
concave = [
    # sqrt(c*x + w)
    atom.QC_sqrt(c*x + w),
    # sqrt(w*y)
    atom.QC_geo_mean(x,y)
]

# TODO: add more nonconvex test cases
nonconvex = [   atom.QC_sqrt(a + atom.QC_square(x)), 
                c*x + u*atom.QC_square(x),
                atom.QC_square(D*E*z) + atom.QC_pow_rat(x, Number(3), Number(4)),
                atom.QC_sqrt(atom.QC_square(x)) ]

not_nonconvex = lambda x: not isnonconvex(x)
not_constant = lambda x: not isconstant(x)
not_concave = lambda x: not isconcave(x)
not_convex = lambda x: not isconvex(x)
not_affine = lambda x: not isaffine(x)


to_str = {
    isconvex : "convex",
    isconcave: "concave",
    isconstant: "constant",
    isaffine: "affine",
    isnonconvex: "nonconvex",
    not_nonconvex: "!nonconvex",
    not_constant: "!constant",
    not_concave: "!concave",
    not_convex: "!convex",
    not_affine: "!affine"
}


def check(is_curvature, x):
    print x
    print "Expected", x, "to be", to_str[is_curvature]
    print "Got", x.curvature
    assert is_curvature(x)
    
def test_vexities():
    for expr in constants:
        yield check, isconstant, expr
        yield check, isaffine, expr
        yield check, isconvex, expr
        yield check, isconcave, expr
        yield check, not_nonconvex, expr
    
    for expr in affine:
        yield check, not_constant, expr
        yield check, isaffine, expr
        yield check, isconvex, expr
        yield check, isconcave, expr
        yield check, not_nonconvex, expr

    for expr in convex:
        yield check, not_constant, expr
        yield check, not_affine, expr
        yield check, isconvex, expr
        yield check, not_concave, expr
        yield check, not_nonconvex, expr
    
    for expr in concave:
        yield check, not_constant, expr
        yield check, not_affine, expr
        yield check, not_convex, expr
        yield check, isconcave, expr
        yield check, not_nonconvex, expr
    
    for expr in nonconvex:
        yield check, not_constant, expr
        yield check, not_affine, expr
        yield check, not_convex, expr
        yield check, not_concave, expr
        yield check, isnonconvex, expr
        
        