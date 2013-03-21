from scoop import Scoop
import cvxopt as o
import numpy as npy

if __name__ == '__main__':
    
    print "Creating data."
    n = 100    # number of variables
    m = 10      # number of factors

    mu = o.exp(o.normal(n))
    D = o.spdiag(o.sqrt(o.uniform(n,b=2.0)))
    F = o.normal(n,m)
    gamma = 1
    
    p = Scoop()
    
    # a Scoop model is specified by strings
    #   the parser parses each model line by line and builds an internal
    #   representation of an SOCP
    p.rewrite(
        """
        variable x vector
        parameter mu vector
        parameter gamma positive
        parameter F matrix
        parameter D matrix
        maximize (mu'*x - gamma*(square(norm(F'*x)) + square(norm(D*x))))
            sum(x) == 10
        """
    )
    
    # you can also add to the problem spec line by line if you wanted to
    # but you have to be careful to reference variables or parameters that
    # you have previously declared
    p.rewrite("x >= 0")
    
    npy.savetxt('mu', npy.matrix(mu))
    (Dp,Di,Dx) = D.CCS
    npy.savetxt('Dx', npy.matrix(Dx))
    npy.savetxt('F', npy.matrix(F))
    
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
    (c, G, h, free_lens, lp_lens, soc_lens) = f3(x = n, mu = mu, D = D, F = F, gamma = gamma)
    npy.savetxt('Gj', npy.matrix(G.J))
    npy.savetxt('Gi', npy.matrix(G.I))
    npy.savetxt('Gx', npy.matrix(G.V))
    
    # generate a function to spit out the raw data
    f4 = p.generate_fixed_matrix(3)
    (c, G, h, free_lens, lp_lens, soc_lens) = f4(x = n, mu = mu, D = D, F = F, gamma = gamma)
    (Gp,Gi,Gx) = G.CCS
    npy.savetxt('Gj_fx', npy.matrix(G.J))
    npy.savetxt('Gi_fx', npy.matrix(G.I))
    npy.savetxt('Gx_fx', npy.matrix(G.V))
    
    # now, use a first order solver to solve the problem
    f5 = p.generate_pdos(VERBOSE=True,NORMALIZE=True,ALPHA=1.8)
    sol3 = f5(x = n, mu = mu, D = D, F = F, gamma = gamma)
    
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

    
