cvx_begin
    variable x(2)
    minimize (x(1) + quad_over_lin((x(1) - x(2)),sqrt(x(2))))
    x(1) <= u
cvx_end