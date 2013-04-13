from scoop import Scoop
from scipy.linalg import cholesky, eigvals, svdvals
import cvxopt as o
import numpy as npy
import time

if __name__ == '__main__':
    
    print "Creating data."
    n = 5       # states
    m = 2       # inputs
    T = 10      # horizon
    
    o.setseed(2)

    Q = o.normal(n,n)
    Q = Q.trans()*Q
    R = o.normal(m,m)
    R = R.trans()*R + o.spdiag(o.matrix(npy.ones(m)))
    Q = o.matrix(cholesky(Q))
    R = o.matrix(cholesky(R))
    

    A = o.normal(n,n)
    s = max(abs(eigvals(A)))
    A = A/s
    B = o.normal(n,m)
    q = max(abs(svdvals(B)))
    B = 1.1*B/q
    xinit = 5*o.normal(n,1)
    
    # TODO: add "/" for constants or something...
    
    p = Scoop()
    
    problem = ["parameter Q matrix",
               "parameter R matrix",
               "parameter A matrix",
               "parameter B matrix",
               "parameter xinit vector"]
    data = {'Q': Q, 'R':R, 'A':A, 'B':B, 'xinit':xinit}
    for i in range(T):
        problem += ["variable x%i vector" % i,
                    "variable u%i vector" % i]
        data['x%i' % i] = n  
        data['u%i' % i] = m  
    problem += ["variable x%i vector" % T]  # final
    data['x%i' % T] = n
    
    problem += ["x0 == xinit"]
    for i in range(T):
        problem += ["x%i == A*x%i + B*u%i" % (i+1,i,i),
                    "norm_inf(u%i) <= 1" % i]
        
    objective = []
    for i in range(T):
        objective += ["square(norm(Q*x%i)) + square(norm(R*u%i))" % (i,i)]
    
    objective += ["square(norm(Q*x%i))" % T]
    problem += ["minimize 0.5*(" + ' + '.join(objective) + ")"]
    p.rewrite('\n'.join(problem))
        
    f = p.generate_ecos()
    t1 = time.time()
    sol = f(**data)
    t2 = time.time()
    print 'took %0.3f ms' % ((t2 - t1)*1000.0)
    
    f5 = p.generate_pdos(VERBOSE=True,NORMALIZE=False,ALPHA=1.8, MAX_ITERS=10000)
    t1 = time.time()
    sol3 = f5(**data)
    t2 = time.time()
    print 'took %0.3f ms' % ((t2 - t1)*1000.0)
    
    print sol['u0']
    print sol['u2']

    
    
    # f2 = p.generate()
#     t1 = time.time()
#     sol2 = f2(x = n, mu = mu, D = D, F = F, gamma = gamma)
#     t2 = time.time()
#     print 'took %0.3f ms' % ((t2 - t1)*1000.0)
    
    
    
