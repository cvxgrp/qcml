#!/usr/bin/env python
from qcml import QCML
import numpy as np
from numpy.random import randn


if __name__ == '__main__':

    print "Creating data."
    n = 2      # number of features
    m = 100   # number of examples
    X = randn(m,n) - 1
    Y = randn(m,n) + 1
    gamma = 1


    print "Creating SVM problem."

    # a QCML model is specified by strings
    #   the parser parses each model line by line and builds an internal
    #   representation of an SOCP
    s = """
    dimensions m n
    variable a(n)
    variable b
    parameter X(m,n)      # positive samples
    parameter Y(m,n)      # negative samples
    parameter gamma positive
    minimize (norm(a) + gamma*sum(pos(1 - X*a + b) + pos(1 + Y*a - b)))
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

    raw_input("press ENTER to generate operator....")
    p.codegen("operator")
    other = p.prob2socp(params=locals())

    x = randn(204,)
    y = randn(403,)

    raw_input("press ENTER to check operator G does the same thing as matrix G....")
    print "G norm diff", np.linalg.norm(socp_data['G'] * x - other['G'](x))
    print "G trans norm diff", np.linalg.norm(socp_data['G'].T * y - other['GT'](y))

