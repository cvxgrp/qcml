#!/usr/bin/env python
""" Continuous basis pursuit example.  This formulation is targeted at the 1D
    polar formulation for CBP.

    For details see: http://www.cns.nyu.edu/pub/lcv/Ekanadham11c-preprint.pdf
"""

from argparse import ArgumentParser
from qcml import QCML

if __name__ == "__main__":
    parser = ArgumentParser(description='Continuous Basis Pursuit QCML example')
    parser.add_argument('-m', type=int, help='Length of waveform (samples)')
    parser.add_argument('-n', type=int, help='Number of templates in dictionary')
    parser.add_argument('-c', '--codegen', help='Codegen type to use (python, matlab, or C; default python)', default='python')
    args = parser.parse_args()
    m, n = (args.m, args.n)

    print "Running CBP example...."

    # TODO: takeaways from this example: "diag" constructor?

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
        parameter radii(n,n)    # diagonal matrix
        parameter rctheta(n,n)  # diagonal matrix
        minimize noise*norm(data - (dictc*c + dictu*u + dictv*v)) + lambda'*c
        subject to
          # || (u[i], v[i]) || <= radii_i * c_i
          # norm([u_i v_i]) <= radii_i*c_i
          # norm(x,y) applies norm across rows of the matrix [x y]
          norm(u,v) <= radii*c

          # rctheta is going to be a diagonal matrix
          # rctheta[i]*c[i] <= u[i] implemented with rctheta a diag matrix
          rctheta*c <= u
    """)

    # More natural formulation would be:
    # minimize 1/(sqrt(2)*noisesigma) * (data - (dictc*c + dictu(u + dictv*v)) + lambda'*c)
    # subject to
    #   sqrt(u.^2 + v.^2) <= radii.*c
    #   radii.*cos(theta) <= u

    raw_input("press ENTER to canonicalize....")
    p.canonicalize()

    raw_input("press ENTER to generate code....")
    if m and n: p.dims = {'m': m, 'n': n}
    p.codegen(args.codegen)

    raw_input("press ENTER for raw code....")
    p.printsource()

    #socp_data = p.prob2socp(params=locals())
    #import ecos
    #sol = ecos.ecos(**socp_data)
    #my_vars = p.socp2prob(sol['x'], sol['y'], sol['z'])
    #pr.disable()
    #ps = pstats.Stats(pr)
    #ps.sort_stats('cumulative').print_stats(.5)
