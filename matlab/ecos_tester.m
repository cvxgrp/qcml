function ecos_tester()
    addpath(pwd)
    randn('state', 0);
    rand('state', 0);
    cvx_precision best
    cvx_quiet false
    cvx_solver sedumi

    try 
        run_test('least_squares'); fprintf('\n');
        run_test('geometric_mean'); fprintf('\n');
        run_test('lp'); fprintf('\n');
        run_test('quadratic_over_linear'); fprintf('\n');
        run_test('inv_prob'); fprintf('\n');
        run_test('min_max'); fprintf('\n');
        run_test('robust_ls'); fprintf('\n');
        run_test('ecos_mpc'); fprintf('\n');
        run_test('lasso'); fprintf('\n');
        run_test('portfolio'); fprintf('\n');
        run_test('svm'); fprintf('\n');
        run_test('chebyshev'); fprintf('\n');


    catch e
        cd ..   % return to top level directory
        e.message
    end
    rmpath(pwd)
end

function run_test(directory)
    disp([directory ' problem'])
    
    cd(directory)
    data;
    tic;
    eval(directory)
    fprintf('  cvx solve time %f\n', toc);
    v1 = cvx_optval;
    x_cvx = x;
    

    tic;
    [status, result] = system(['../../src/efe --ecos ' directory '.prob']);
    fprintf('  ecos rewrite time %f\n', toc);

    clear x
    try
        eval(result);
        
%         figure(1); subplot(1,2,1); spy(A_); subplot(1,2,2); spy(A1)
%         figure(2); subplot(1,2,1); spy(G_); subplot(1,2,2); spy(G1)
%        pause
%         [xtmp, ind, c_, A_, b_, G_, h_] = ecos_presolve(c_, A_, b_, G_, h_);
%         [sol, info_2] = ecos(full(c_), G_, full(h_), dims, A_, full(b_));
%      
%         xtmp(ind) = sol;
    catch e
        result
        % info_% only when running with paris
        throw(e)
    end
%     C = [eye(size(A_,2)) A_'; A_ -eye(size(A_,1))];
%     [L,D, p] = ldl(C);
%     spy(L)

    v2 = ecos_optval;   % need this with CVX, don't need it without
    
    x_ecos = zeros(length(x_cvx),1);
    try
        for i = 1:length(x_cvx),
            x_ecos(i) = eval(['x' num2str(i)]);
        end
    catch e
        if(exist('a','var') && exist('b','var'))
            % for svm
            x_ecos = [a; b];
        else
            x_ecos = x;
        end
    end
    fprintf('  ||x error|| = %f\n', norm(x_cvx - x_ecos));
    fprintf('  objval error = %f\n', v1 - v2);
    
    if(norm(x_cvx - x_ecos) <= 1e-5 && abs(v1 - v2) <= 1e-5)
        fprintf('PASS\n');
    else
        fprintf('FAIL\n');

        result
        info_% only when running with paris
        [x_cvx full(x_ecos)]
        %cvx_status
        [v1 v2]

        pause
    end
    cd ..
end

%
% A defined as --
% rowidx = 
% vals = 
% col_startidx = [0,4,6,7,9,10]
%
% G(14:..., 10:...) = A
% G_rowidx
% G_vals
% G_col_startidx =
%

% i can store each column independently?
%
% in C code, the matrix A is "static" (just malloc the memory needed for it,
% since you'll know nnz at runtime)--static in the sense that the memory
% allocation only occurs once.
%
% elements should be copied based on "nnz" per "col" of inserted elements
%

% this means params needs to store in CCS and also have m,n vals for
% matrices...

% going to look something like this... (assuming A is an n x 2 matrix)
% G_vals = [...., params.A.col(0), other stuff, params.A.col(1), .... ]
% so G_vals and G_rowidx = #constants + nnz(A) (which is at the end of the
% matrix)
%
% can emit static stuff, like G_vals[0] = 1, G_vals[1] = 3, etc.
%
% when having to *copy* over a matrix
%

% memcpy code...
% for(i = (start idx of col 0):(start idx of col 1) - 1)
%   G_rowidx[k] = A_rowidx[i] + row offset
%   G_vals[k] = A_vals[i]
%   k++
% basically, this is
%
% XXX/TODO: THIS IS THE PIECE OF CODE TO USE (and, yes, all caps is warranted here!) 
% =========================================================================
% memcpy(from &A_rowidx[i], to &G_rowidx[k], length=(start idx of col 1) - (start idx of col 0))
% increment k by length
% =========================================================================

%
% depending on the compiler (and the machine architecture), memcpy could be
% faster than for loops, and for loops could be faster than memcpy.
%
% for simplicity, we'll stick with memcpy's (since, on average, it's
% fastest, without needing to play with compiler flags).
%
