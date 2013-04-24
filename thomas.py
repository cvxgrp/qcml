from scoop import Scoop
# import cvxopt as o
# import numpy as npy
# import time

p = Scoop()

# if we provide "(m,n)" capabilities, we will also need to provide

p.rewrite(
    """
    variable u1 vector
    variable u2 vector
    variable a vector
    variable b vector
    
    parameter M matrix      # mass matrix
    parameter R1 matrix     # 1st row of R
    parameter R2 matrix     # 2nd row of R
    parameter D matrix      # difference matrix
    parameter delta vector  # constant offset
    parameter c matrix      # coefficient matrix
    parameter dtheta positive
    parameter b0            # v_0 / (norm(S_1 - S_0)^2)
    
    parameter sel0 vector   # selects the 1st element
    parameter sel1N vector      # select 1:N-1
    parameter sel2end vector    # select 2:end
    
    D*b == 2*dtheta*a
    b >= 0
    sel0'*b == dtheta*b0
    
    R1*u1 + R2*u2 == M*a + c*(D*b) + delta
    
    norm(u1,u2) <= 1
    u1 <= 0.5
    
    minimize (2*dtheta*sum(inv_pos( sqrt(sel1N'*b) + sqrt(sel2end'*b)  ) ))
    """
)

print p

# generate matlab code (doesn't require any information)
