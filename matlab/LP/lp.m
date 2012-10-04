cvx_begin
    variable x(n)
    minimize (c'*x)
    A*x == b
    x >= 0
cvx_end