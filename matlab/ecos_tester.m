function ecos_tester()
    addpath(pwd)
    randn('state', 0);
    cvx_precision best
    cvx_quiet true
    cvx_solver sdpt3

    % 1 - minimization problem, -1 - maximization problem
    try 
        run_test('least_squares'); fprintf('\n');
        run_test('geometric_mean'); fprintf('\n');
        run_test('lp'); fprintf('\n');
        run_test('quadratic_over_linear'); fprintf('\n');
        run_test('inv_prob'); fprintf('\n');
        run_test('min_max'); fprintf('\n');
        run_test('robust_ls'); fprintf('\n');
        run_test('ecos_mpc'); fprintf('\n');
        run_test('lasso'); %fprintf('\n');

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
    
    tic;
    [status, result] = system(['../../src/ProbToCVX --ecos ' directory '.prob']);
    fprintf('  ecos rewrite time %f\n', toc);

    try
        eval(result);
    catch e
        result
        info_% only when running with paris
        throw(e)
    end
%     C = [eye(size(A_,2)) A_'; A_ -eye(size(A_,1))];
%     [L,D, p] = ldl(C);
%     spy(L)

    v2 = ecos_optval;   % need this with CVX, don't need it without

    x_ecos = zeros(length(x),1);
    for i = 1:length(x),
        x_ecos(i) = eval(['x' num2str(i)]);
    end
    
    
    fprintf('  ||x error|| = %f\n', norm(x - x_ecos));
    fprintf('  objval error = %f\n', v1 - v2);
    
    if(norm(x - x_ecos) <= 1e-5 && abs(v1 - v2) <= 1e-5)
        fprintf('PASS\n');
    else
        fprintf('FAIL\n');

        result
        info_% only when running with paris
        [x x_ecos]
        %cvx_status
        [v1 v2]
        pause
    end
    cd ..
end

