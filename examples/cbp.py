#!/usr/bin/python
from qcml import QCML

import cvxopt as o
from cvxopt import solvers
import cProfile, pstats

if __name__ == "__main__":
    n = 56 # number of waveforms
    m = 83 # waveforms length

    pr = cProfile.Profile()

    p = QCML(debug=True)
    p.parse("""
        dimensions m n
        variable c(n)
        variable u(n)
        variable v(n)
        parameter noise positive
        parameter lambda(n)
        parameter data(m)
        parameter dictc(m,n)
        parameter dictu(m,n)
        parameter dictv(m,n)
        parameter radii(n)
        parameter rctheta(n)
        minimize noise*norm(data - (dictc*c + dictu*u + dictv*v)) + lambda'*c
        subject to
          norm([u_i v_i]) <= radii_i*c_i
          rctheta_i*c_i <= u_i
    """)

    # More natural formulation would be:
    # minimize 1/(sqrt(2)*noisesigma) * (data - (dictc*c + dictu(u + dictv*v)) + lambda'*c)
    # subject to
    #   sqrt(u.^2 + v.^2) <= radii.*c
    #   radii.*cos(theta) <= u

    solvers.options['abstol'] = 1e-9
    solvers.options['reltol'] = 1e-9

    pr.enable()
    p.canonicalize()
    p.dims = {'n': n, 'm': m}
    p.codegen("python")
    print p.prob2socp.source
    #socp_data = p.prob2socp(params=locals())
    #import ecos
    #sol = ecos.ecos(**socp_data)
    #my_vars = p.socp2prob(sol['x'])
    #pr.disable()
    #ps = pstats.Stats(pr)
    #ps.sort_stats('cumulative').print_stats(.5)
