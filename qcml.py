from scoop import QCParser

if __name__ == "__main__":
    p = QCParser()
    y = p.parse("""
        dimensions m n
        variable z
        parameter b
        parameter gamma positive
        parameter A(m,n)
        variable x(m)
        parameter c(m)
        
        variables y(31) a(1,5)
        
        82 + 6/3 == 1
        dimension p
        8 + gamma <= 5
        8+x + z - b+1 == 3
        8+x-1+z >= 1
        3*(21+x)-2*x*5+8-(4+1) == 4
        (b+b)*z <= x
        (b*z - 2) - (b*z - 2) >= z
        (b + b + b)*(x + z + b) == 0
        -5 == 5
        -(x + 1) == 0
        A'*x - b == 0 
        --x == 0
        quad_over_lin(x,y,z) <= 1

        minimize c'*x + b
        subject to
        """)
    
    print y
    y.show()