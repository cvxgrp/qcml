# atoms from separate files

import qc_geo_mean
import qc_huber
import qc_inv_pos
import qc_max
import qc_min
import qc_neg
import qc_norm_inf
import qc_norm1
import qc_pos
import qc_pow_rat
import qc_quad_over_lin
import qc_sqrt
import qc_square_over_lin
import qc_square


# build dictionary of methods (matching with identifiers)

# convention is ATOM.attributes() and ATOM.rewrite()
""" This is a dictionary of modules.

    In every module, you must have defined two functions:
        attributes :: [arg] -> (sign, vexity, shape)
        rewrite :: [arg] -> Program

    Atoms defined using other atoms....
        whenever "square(x)" is called in the AST, it should be replaced with
        "quad_over_lin(x,Constant(1.0))"

        but the AST node should still be called "square"

        but problem occurs when you have >= etc. in the Atom definition...

        maybe rewrite will just replace AST nodes?

        so you need to chain rewrites? like

        x^4 = x^2*x^2 = quad_over_lin(x,1) * quad_over_lin(x,1)?

        or should you just traverse the tree twice?

        meh--every atom should just have an SOC definition

        or can we just rewrite as we parse? nah. too crazy.

    Eventually, atoms will be defined in QCML. We'll provide a mechanism to
    define atoms. Need some way to pre-parse and pre-load the "program" tree
    into a table.
"""
atoms = \
    {
        # 'abs': abs_,
        'geo_mean': qc_geo_mean,
        'huber': qc_huber,
        'inv_pos': qc_inv_pos,
        'max': qc_max,
        'min': qc_min,
        'neg': qc_neg,
        'norm_inf': qc_norm_inf,
        # 'norm': norm,
        # 'norm2': norm,
        'norm1': qc_norm1,
        'pos': qc_pos,
        'pow_rat': qc_pow_rat,
        'quad_over_lin': qc_quad_over_lin,
        'square_over_lin': qc_square_over_lin,
        'sqrt': qc_sqrt,
        'square': qc_square,
        # 'sum': sum_
    }

from qc_base import norm, norm_rewrite, abs_, abs_rewrite

pow_func = None



