function txt = cg_dump_conelpproblem(c,G,h,dims,A,b,fn)
% Dump a conic problem in sparse format (A and G) to a text file.

[m,n] = size(G);
p = size(A,1);

epsilon = 1e-7;
K0 = sparse([ epsilon*eye(n),    A',         G';
       A,     -epsilon*eye(p),  zeros(p,m);
       G,     zeros(m,p),  -eye(m)]);

txt = {};
txt = [txt; 'idxint n = ',num2str(n),';'];
txt = [txt; 'idxint m = ',num2str(m),';'];
txt = [txt; 'idxint p = ',num2str(p),';'];
txt = [txt; 'idxint l = ',num2str(dims.l),';'];
txt = [txt; 'idxint ncones = ',num2str(length(dims.q)),';'];
txt = [txt; cg_mat2c('pfloat','c',c,'%20.18e')];
txt = [txt; cg_mat2c('pfloat','h',h,'%20.18e')];
txt = [txt; cg_mat2c('idxint','q',dims.q,'%d')];
txt = [txt; cg_dump_spmat(G,'G','pfloat','idxint')];
txt = [txt; cg_dump_spmat(A,'A','pfloat','idxint')];
%txt = [txt; cg_dump_spmat_symmetric_upper_part(K0,'K0','pfloat','idxint')];
txt = [txt; cg_mat2c('pfloat','b',b,'%20.18e')];

% write file
cg_dumpfile(fn,txt);