from scoop import Scoop
import cvxopt as o

if __name__ == '__main__':
    
    p = Scoop()
    
    # a Scoop model is specified by strings
    #   the parser parses each model line by line and builds an internal
    #   representation of an SOCP
    p.rewrite(
        """
        variable x vector
        minimize sum(x)
            norm(x) <= 3
        """
    )
    
    # you can also add to the problem spec line by line if you wanted to
    # but you have to be careful to reference variables or parameters that
    # you have previously declared
    p.rewrite("x >= -2")
    
    print p
    
    raw_input("press ENTER to continue....")
    
    # generate a solver where the size of a second-order cone is *fixed* to 
    # length 3
    f = p.generate_fixed_soc(3)
    # solve the problem with x of length 5
    sol1 = f(x = 5)
    
    # generate a generic solver (CVXOPT) for the second-order cone program
    f2 = p.generate()
    # solve the problem with x of length 5
    sol2 = f2(x = 5)
    
    # generate a function to spit out the raw data
    # f3 = p.generate_matrix()
    # (c, G, h, free_lens, lp_lens, soc_lens) = f3(x = 5)
    
    # now, use a first order solver to solve the problem
    f4 = p.generate_pdos(False)
    sol3 = f4(x = 5)
    
    # compare the solutions
    print sol1['x']
    print sol2['x']
    print sol3['x']
    #print sol['x']
    
    raw_input("now solve the same problem with different length variable; press ENTER to continue....")
    # now try solving the same problem but with different length x's
    for l in range(1,10):
        sol = f2(x = l)
        print sol['x']

    
