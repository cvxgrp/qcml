from scoop import Scoop
import cvxopt as o
import numpy as npy

if __name__ == '__main__':
    
    print "Creating data."
    n = 2      # number of features
    m = 100   # number of examples
    X = o.normal(m,n, -2)
    Y = o.normal(m,n, 2)
    gamma = 1
    
    print "Rewriting SVM"
    p = Scoop()
    
    # a Scoop model is specified by strings
    #   the parser parses each model line by line and builds an internal
    #   representation of an SOCP
    p.rewrite(
        """
        variable a vector
        variable b scalar
        parameter X matrix      # positive samples
        parameter Y matrix      # negative samples
        parameter gamma positive
        minimize (norm(a) + gamma*sum(pos(1 - X*a + b) + pos(1 + Y*a - b)))
        """
    )
    
    print p
    
    raw_input("press ENTER to continue....")
    
    # # generate a solver where the size of a second-order cone is *fixed* to 
    # # length 3
    # f = p.generate_fixed_soc(3)
    # # solve the problem with x of length 5
    # sol1 = f(x = n, mu = mu, D = D, F = F, gamma = gamma)
    # 
    # # generate a generic solver (CVXOPT) for the second-order cone program
    # f2 = p.generate()
    # # solve the problem with x of length 5
    # sol2 = f2(x = n, mu = mu, D = D, F = F, gamma = gamma)
    
    # generate a function to spit out the raw data
    f3 = p.generate_matrix()
    (c, G, h, free_lens, lp_lens, soc_lens) = f3(a = n, X = X, Y = Y, gamma = gamma)
    npy.savetxt('Gj', npy.matrix(G.J))
    npy.savetxt('Gi', npy.matrix(G.I))
    npy.savetxt('Gx', npy.matrix(G.V))
    
    npy.savetxt('c', npy.matrix(c))
    npy.savetxt('h', npy.matrix(h))
    
    print free_lens
    print lp_lens
    print soc_lens
    
    # generate a function to spit out the raw data
    f4 = p.generate_fixed_matrix(3)
    (c, G, h, free_lens, lp_lens, soc_lens) = f4(a = n, X = X, Y = Y, gamma = gamma)
    (Gp,Gi,Gx) = G.CCS
    npy.savetxt('Gj_fx', npy.matrix(G.J))
    npy.savetxt('Gi_fx', npy.matrix(G.I))
    npy.savetxt('Gx_fx', npy.matrix(G.V))
    
    # now, use a first order solver to solve the problem
    f5 = p.generate_pdos(VERBOSE=True,NORMALIZE=True,ALPHA=1.8,MAX_ITERS=10000)
    sol3 = f5(a = n, X = X, Y = Y, gamma = gamma)
    
    # compare the solutions
    # print sol1['x']
    # print sol2['x']
    # print sol3['x']
    #print sol['x']

    # raw_input("now solve the same problem with different length variable; press ENTER to continue....")
    # # now try solving the same problem but with different length x's
    # for l in range(1,10):
    #     sol = f2(x = l)
    #     print sol['x']

    
