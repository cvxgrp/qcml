function matstr = cg_dumpmat(M,elsep,fstr)
% CG_DUMPMAT Convert matrix to string.
%    MATSTR = CG_DUMPMAT(M) converts the matrix M to a string which is
%    returned in MATRSTR. The elements of M will be seperated by commas.
%
%    MATSTR = CG_DUMPMAT(M,ELSEP) converts the matrix M to a string which
%    is returned in MATRSTR. The elements of M will be seperated by the
%    string ELSEP.
%
%    MATSTR = CG_DUMPMAT(M,ELSEP,FORMATSTR) converts the matrix M to a
%    string with the elements of M seperated by string ELSEP and formatted
%    according to FORMATSTR.
% 
% see also CG_DUMPFILE

dimi = size(M,1);
dimj = size(M,2);
matstr = cell(dimi,1);

if( nargin < 3 ) 
    fstr = '%20.18e';
end;

if( ~exist('elsep','var') )
    elsep = ',';
end

for i = 1:dimi
    temp = num2str(M(i,1),fstr);    
    for j = 2:dimj
        temp = [temp,elsep,' ',num2str(M(i,j),fstr)];
    end    
    if( i < dimi )
        temp = [temp,elsep];
    end
    matstr{i} = temp;
end

if( dimi == 1 )
    matstr = matstr{1};
end