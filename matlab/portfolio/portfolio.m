cvx_begin
    variable x(n)

    sigma = F*F' + D*D';
    maximize (mu'*x - gamma*(x'*sigma*x))
    subject to
        sum(x) == B
        x >= 0
cvx_end
% x
% cvx_optval
% 
% cvx_begin
%     variable x(n)
%     variables u v t
% 
% 
%     maximize (mu'*x - gamma*t)
%     subject to
%         norm(F'*x) <= u
%         norm(D*x) <= v
%         norm([u;v]) <= t
%         c'*x == C
%         x >= 0
% cvx_end
% x
% cvx_optval
