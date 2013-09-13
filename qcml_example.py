#!/usr/bin/env python
from qcml import QCML
import numpy as np
from numpy.random import randn
import cProfile, pstats

if __name__ == "__main__":
    n = 2      # number of features
    m = 100   # number of examples
    X = randn(m,n) - 1
    Y = randn(m,n) + 1
    gamma = 1

    p = QCML(debug=True)
    p.parse("""
        dimensions m n
        variable a(n)
        variable b
        parameter X(m,n)      # positive samples
        parameter Y(m,n)      # negative samples
        parameter Z(m,n)
        parameter W(m,n)
        parameter gamma positive
        parameter c(n)
        # variables x(n) z
        #
        # minimize huber(sum(x)) - sqrt(z)
        #     x + z == 5
        #     x + z == 5
        #     # x(:,i+1) == A(:,:,i)*x(:,i) + B*u(:,i) for i = 1,...,T
        #     # sum_{ij in E}
        #     #
        #     # matrix X
        #     # A*X is map(A*x, X)
        #     # X*A
        minimize (norm(a) + gamma*sum(pos(1 - X*a + b) + pos(1 + Y*a - b)))
        # minimize c'*a
        #    norm(X*a,Y*a,Z*a, W*a) <= 1
    """)

    # TODO: sum(norms(X))
    # A*x
    #
    p.solve()

    pr = cProfile.Profile()
    pr.enable()

    p.canonicalize()
    p.dims = {'n': n, 'm': m}
    p.codegen("python")
    print p.prob2socp.source
    socp_data = p.prob2socp(params=locals())
    import ecos
    sol = ecos.solve(**socp_data)
    my_vars = p.socp2prob(sol['x'])

    pr.disable()
    ps = pstats.Stats(pr)
    ps.sort_stats('cumulative').print_stats(.5)

