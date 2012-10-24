m = 100;
n = 20;
p = 5;
A = randn(m,n);
C = randn(p,n);
d = randn(p,1);


xtrue = randn(n,1);
b = A*xtrue + 0.1*randn(m,1);

e = rand;