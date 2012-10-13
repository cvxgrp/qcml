function txt = cg_mat2c(type,name,M,fstr)
% CG_MAT2C Convert MATLAB matrix into C-code style variable declaration.
%    TXT = CG_MAT2C(TYPE,NAME,M) returns a cell array of strings which can
%    be directly used in the programming language C for defining the matrix
%    M of type TYPE and assigning the identifier NAME.
%
% CG_MAT2C Convert MATLAB matrix into C-code style variable declaration.
%    TXT = CG_MAT2C(TYPE,NAME,M,FSTR) as above, just with user-specified
%    formatting.
%
% (c) Alexander Domahidi, IfA, ETH Zurich, 2009.
%
% See also CG_DUMPMAT, CG_DUMPFILE, CG_DUMPFILTER

if( nargin < 4 )
    fstr = '%20.18e';
end

[m,n] = size(M);
isvec = 0;

if( m==1 )    
    isvec = 1;
end

if( n == 1 )
    isvec = 1;
    M = M';
end

if( isvec ) 
    txt = [type,' ',name,'[',num2str(numel(M)),'] = {',cg_dumpmat(M,',',fstr),'};'];
else
    txt{1,1} = [type,' ',name,'[',num2str(numel(M)),'] = {'];
    txt = [txt; cg_dumpmat(M,',',fstr)];
    txt{end+1,1} = '};';
end