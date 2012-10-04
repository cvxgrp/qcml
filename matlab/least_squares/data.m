m = 3;
n = 2;
A = randn(m,n);

xtrue = randn(n,1);
b = A*xtrue + 0.1*randn(m,1);


a11 = A(1,1); a12 = A(1,2);
a21 = A(2,1); a22 = A(2,2);
a31 = A(3,1); a32 = A(3,2);

b1 = b(1);
b2 = b(2);
b3 = b(3);