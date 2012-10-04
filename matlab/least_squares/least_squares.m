cvx_begin
    variable x(n)
    minimize (sum_square(A*x - b))
cvx_end

