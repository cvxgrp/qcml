function txt = cg_dump_spmat(G,name,ftype,itype,fn)
% Dumps a sparse matrix in column compressed format into a text file.
%
% USAGE: CG_DUMP_SPMAT(G,NAME,FN) writes the matrix in column compressed 
%        format in C such that it can be directly used by Paris sparse
%        matrix data structure spmat

[m,n] = size(G);
G = sparse(G);

% find jc, ir and pr
jc = NaN(1,n+1);
jc(1) = 1;
k = 1;
for j = 1:n    
    jc(j) = k;
    for i = 1:m
        v = full(G(i,j));
        if( v ~= 0 )
            ir(k) = i; 
            pr(k) = v;
            k = k+1; 
        end
    end
end
jc(n+1) = k;


% create text
txt{1,1}    = [itype,' ',name,'jc[',num2str(length(jc)),'] = {',cg_dumpmat(jc-1,',','%d'),'};'];
txt{end+1,1}= [itype,' ',name,'ir[',num2str(length(ir)),'] = {',cg_dumpmat(ir-1,',','%d'),'};'];
txt{end+1,1}= [ftype,' ',name,'pr[',num2str(length(pr)),'] = {',cg_dumpmat(pr),'};'];
% txt{end+1,1}= [itype,' ',name,'m = ',num2str(m),';'];
% txt{end+1,1}= [itype,' ',name,'n = ',num2str(n),';'];
% txt{end+1,1}= [itype,' ',name,'nnz = ',num2str(k),';'];

% dump to c file
if( nargin == 5 )
    cg_dumpfile(fn,txt);
end