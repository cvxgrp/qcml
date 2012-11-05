function [x, ind, c,A,b,G,h] = ecos_presolve(c,A,b,G,h)
[m,n] = size(A);

x = sparse(n,1);

spy(A)
% for each row...
for i = 1:m,
   if(nnz(A(i,:)) == 2)
       i
       A(i,:)
       b(i)
   end
   if(nnz(A(i,:)) == 1)
       ind = find(A(i,:));
       val = A(i,ind);
       
       col = A(:,ind);
       e = b(i);
       b = b - (col*e/val);
       
       x(ind) = e/val;
       
       col = G(:,ind);
       h = h - (col*e/val);
       
       G(:,ind) = 0;
       A(:,ind) = 0;
   end
end


zs = find( (sum(spones(A)) + sum(spones(G))) > 0);
G = G(:,zs);
A = A(:,zs);
c = c(zs);
ind = zs';

zs = find(sum(spones(A),2) > 0);
A = A(zs,:);
b = b(zs);
end