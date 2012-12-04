
a = randn(3,1);
a
% this computes sum(a.^(1/3)) when a is positive
% it *ought* to be infeasible when any entry of a is negative

c_ = sparse(28,1);
c_(28) = -1;
b_ = sparse(19,1);
b_(11:13) = -0.5*ones(3, 1);
b_(14:16) = -0.5*ones(3, 1);
b_(17:19) = a;
A_ = sparse(19, 28);
A_(1:1, 28:28) = -1*ones(1, 1); A_(1:1, 13:15) = 1*ones(1, 3);
A_(2:4, 25:27) = 1*speye(3, 3); A_(2:4, 1:3) = 1*speye(3, 3); A_(2:4, 22:24) = -1*speye(3, 3);
A_(5:7, 13:15) = 0.5*speye(3, 3); A_(5:7, 25:27) = 0.5*speye(3, 3); A_(5:7, 4:6) = -1*speye(3, 3);
A_(8:10, 13:15) = 0.5*speye(3, 3); A_(8:10, 25:27) = -0.5*speye(3, 3); A_(8:10, 7:9) = -1*speye(3, 3);
A_(11:13, 10:12) = 0.5*speye(3, 3); A_(11:13, 16:18) = -1*speye(3, 3);
A_(14:16, 10:12) = -0.5*speye(3, 3); A_(14:16, 19:21) = -1*speye(3, 3);
A_(17:19, 22:24) = 1*speye(3, 3);
G_ = sparse(27, 28);
G_(1:1:3, 1:3) = -speye(3, 3);
G_(4:1:6, 13:15) = -speye(3, 3);
G_(7:1:9, 22:24) = -speye(3, 3);
G_(10:3:18, 4:6) = -speye(3, 3);
G_(11:3:19, 7:9) = -speye(3, 3);
G_(12:3:20, 10:12) = -speye(3, 3);
G_(19:3:27, 16:18) = -speye(3, 3);
G_(20:3:28, 19:21) = -speye(3, 3);
G_(21:3:29, 13:15) = -speye(3, 3);
h_ = zeros(27, 1);
dims.q = [3,3,3,3,3,3];
dims.l = 9;
[x_codegen, y_, info_] = ecos(full(c_), G_, h_, dims, A_, full(b_));
t0s1 = x_codegen(1:3);
t0s0s0z0 = x_codegen(4:6);
t0s0s0z1 = x_codegen(7:9);
t0s0 = x_codegen(10:12);
t0 = x_codegen(13:15);
t0s0z0 = x_codegen(16:18);
t0s0z1 = x_codegen(19:21);
pa = x_codegen(22:24);
t0s0s0 = x_codegen(25:27);
t1 = x_codegen(28:28);
ecos_optval = -1*info_.pcost;

% check results
comparison = [ecos_optval sum(a.^(1/3))];
tmp = -G_*x_codegen;
positive_components = tmp(1:dims.l);

comparison
positive_components
