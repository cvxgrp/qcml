from scoop import QCML

import cvxopt as o

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
    X = o.normal(m,n, -2)
    Y = o.normal(m,n, 2)
    gamma = 1
    
    p = QCML()
    p.parse("""
        dimensions m n
        variable a(n)
        variable b
        parameter X(m,n)      # positive samples
        parameter Y(m,n)      # negative samples
        parameter gamma positive
        minimize (norm(a) + gamma*sum(pos(1 - X*a + b) + pos(1 + Y*a - b)))
        abs(a) <= 1
    """)
    
    p.rewrite()
    p.codegen("ecos")
    p.prettyprint()
    #s = p.solver(m=m,n=n,X=X,Y=Y,gamma=gamma)
    
    p.codegen("matlab",cone_size=10,m=2,n=2)
    p.prettyprint()
        
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
