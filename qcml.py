from scoop import QCParser, QCRewriter, CVXCodegen, CVXOPTCodegen

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
    p = QCParser()
    y = p.parse("""
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

        """)
    
    if y:
        y.show()
        visit = QCRewriter()
        visit.visit(y)

        print y
        # TODO: before codegen, need to check DCP compliance and that objective is scalar
        codegen = CVXOPTCodegen(visit.replaced_expressions())
        codegen.visit(y)
        f = codegen.codegen()
        s = f(m=1,n=1,A=1,b=1)#,gamma=0.1)
        print s['x']
