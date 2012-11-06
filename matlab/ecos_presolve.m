function [x, ind, c,A,b,G,h, recovery, recoverz] = ecos_presolve(c,A,b,G,h)
[m,n] = size(A);

x = sparse(n,1);
recovery = sparse(n,1);
recoverz = sparse(n,n);


% for each row...
for i = m:-1:1,
   if(nnz(A(i,:)) == 2)
       inds = find(A(i,:));
       indx = inds(1); indy = inds(2);
       
       col1 = A(:,indx);
       col2 = A(:,indy);
       
       val1 = col1(i); val2 = col2(i);
       bval = b(i);
       A(:,indy) = col2 - col1*val2/val1;
       b = b - bval/val1*col1;
       
       col1 = G(:,indx);
       col2 = G(:,indy);
       
       G(:,indy) = col2 - col1*val2/val1;
       h = h - bval/val1*col1;
       
       val = c(indx);
       c(indx) = 0;
       c(indy) = c(indy) - val*val2/val1;
       
       recovery(indx) = bval/val1;
       recoverz(indx,indy) = val2/val1;
       
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

recoverz = recoverz(:,ind);
% spy(G)
% spy(A)
end


