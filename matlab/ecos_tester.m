function ecos_tester()
    randn('state', 0);
    cvx_precision best
    cvx_quiet true
    cvx_solver sdpt3

    % 1 - minimization problem, -1 - maximization problem
    try 
        run_test('least_squares', 1); disp('');
        run_test('geometric_mean', -1); disp('');
        run_test('lp', 1); disp('');
    catch e
        cd ..   % return to top level directory
        e.message
    end
end

function run_test(directory, type)
    disp([directory ' problem'])
    
    cd(directory)
    data;
    tic;
    eval(directory)
    fprintf('  cvx solve time %f\n', toc);
    v1 = cvx_optval;
    
    tic;
    [status, result] = system(['../../src/ProbToCVX ' directory '.prob']);
    fprintf('  ecos rewrite time %f\n', toc);

    eval(result);
    v2 = type*cvx_optval;

    
    x_ecos = zeros(length(x),1);
    for i = 1:length(x),
        x_ecos(i) = eval(['x' num2str(i)]);
    end
    

   fprintf('  ||x error|| = %f\n', norm(x - x_ecos));
    fprintf('  objval error = %f\n', v1 - v2);
    cd ..
end
