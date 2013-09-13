#!/usr/bin/env python
from qcml import QCML
import numpy as np
from numpy.random import randn, rand
from scipy.sparse import spdiags


if __name__ == '__main__':

    print "Creating data."
    n = 100   # number of assets
    m = 10    # number of factors
    mu = np.exp(randn(n))
    F = randn(n,m)
    D = spdiags(rand(n),0,n,n)
    gamma = 1


    print "Creating portfolio problem."

    # a QCML model is specified by strings
    #   the parser parses each model line by line and builds an internal
    #   representation of an SOCP
    s = """
        dimensions m n

        variable x(n)
        parameter mu(n)
        parameter gamma positive
        parameter F(n,m)
        parameter D(n,n)
        maximize (mu'*x - gamma*(square(norm(F'*x)) + square(norm(D*x))))
            sum(x) == 1
            x >= 0
    """
    print s

    raw_input("press ENTER to parse....")
    p = QCML(debug=True)
    p.parse(s)

    raw_input("press ENTER to canonicalize....")
    p.canonicalize()

    raw_input("press ENTER to generate code....")
    p.dims = {'n': n, 'm': m}
    p.codegen("python")

    raw_input("press ENTER to solve with ECOS....")
    socp_data = p.prob2socp(params=locals())
    import ecos
    sol = ecos.solve(**socp_data)


