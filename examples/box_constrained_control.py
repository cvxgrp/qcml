#!/usr/bin/env python
from qcml import QCML
from scipy.linalg import cholesky, eigvals, svdvals
from scipy.sparse import eye
import cvxopt as o
import numpy as np
from numpy.random import randn

if __name__ == '__main__':
    
    print "Creating data."
    n = 5      # states
    m = 2      # inputs
    T = 5      # horizon, TODO: will take time to canonicalize if too large
    
    Q = randn(n,n)
    R = randn(m,m)

    A = randn(n,n)
    s = max(abs(eigvals(A)))
    A = A/s     # ensure stability
    B = randn(n,m)
    q = max(abs(svdvals(B)))
    B = 1.1*B/q # control law is slightly unstable
    
    xinit = 5*randn(n)
    
    print "Creating problem."        
    problem = [
        "dimensions m n",
        "parameters Q(n,n) R(m,m) A(n,n) B(n,m)",
        "parameter xinit(n)"]
    data = {'Q': Q, 'R':R, 'A':A, 'B':B, 'xinit':xinit}
    for i in xrange(T):
        problem += ["variables x%i(n) u%i(m)" % (i,i)]
    problem += ["variable x%i(n)" % T]  # final
    
    problem += ["x0 == xinit"]
    for i in xrange(T):
        problem += ["x%i == A*x%i + B*u%i" % (i+1,i,i),
                    "norm_inf(u%i) <= 1" % i]
        
    objective = []
    for i in xrange(T):
        objective += ["square(norm(Q*x%i)) + square(norm(R*u%i))" % (i,i)]
    
    #objective += ["square(norm(Q*x%i))" % T]
    problem += ["minimize (1/2)*(" + ' + '.join(objective) + ")"]
    
    s = '\n'.join(map(lambda x:'    ' + x, problem))
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
    
    

    
    
    
