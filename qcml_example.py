#!/usr/bin/python
from qcml import QCML

import numpy as np
from numpy.random import randn
#from cvxopt import solvers
import cProfile, pstats

if __name__ == "__main__":

    """

    # 82 + 6/3 == 1
    # dimension p
    # 8 + gamma <= 5
    # 8+x + z - b+1 == 3
    # 8+x-1+z >= 1
    # 3*(21+x)-2*x*5+8-(4+1) == 4
    # (b+b)*z <= x
    # (b*z - 2) - (b*z - 2) >= z
    # (b + b + b)*(x + z + b) == 0
    # -5 == 5
    # -(x + 1) == 0
    # A'*x - b == 0
    # --x == 0
    # x'*x <= 1
    # quad_over_lin(x,z) <= 1
    #
    # minimize c'*x + b
    # subject to
    """

    """
    dimensions m n
    variable z
    parameter b(m)
    parameter gamma positive
    parameter A(m,n)
    variable x(n)
    parameter c(m)


    variables y(31) a(1,5)

    # square(x) <= 1

    minimize sum(square(A*x - b))
    """

    n = 2      # number of features
    m = 100   # number of examples
    X = randn(m,n) - 1
    Y = randn(m,n) + 1
    gamma = 1

    pr = cProfile.Profile()

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
    # solvers.options['abstol'] = 1e-9
    # solvers.options['reltol'] = 1e-9

    # p.canonicalize()
    # p.set_dims({'n': n, 'm': m})
    # p.codegen("pdos")
    # s = p.solver(locals())
    #
    # p.print_canon()
    # p.codegen("cvx")
    #p.prettyprint(True)

    #s = p.solve()

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
    ps.sort_stats('cumtime').print_stats(.5)

#
#     p.codegen("pdos")
#     p.prettyprint(True)
#     s = p.solver(m=m,n=n,X=X,Y=Y,gamma=gamma)
#
#     p.codegen("matlab",cone_size=3,m=100,n=10)
#     p.prettyprint()



    # if y:
    #     #y.show()
    #     visit.visit(y)
    #
    #     print y
    #     # TODO: before codegen, need to check DCP compliance and that objective is scalar
    #     codegen = ECOSCodegen(visit.replaced_expressions())
    #     codegen.visit(y)
    #     codegen.prettyprint(True)
    #
    #     f = codegen.codegen()
    #     #s = f(m=1,n=1,A=1,b=1)#,gamma=0.1)
    #     s = f(m=m,n=n,X=X,Y=Y,gamma=gamma)
    #
    #     print s
    #     print s['a']
    #     print s['b']
