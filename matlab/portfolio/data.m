n = 100;
m = 5;

F = randn(n,m);
Ft = F';
D = diag(sqrt(rand(n,1)));

B = 20;

mu = rand(n,1);
mut = mu';

gamma = 1;