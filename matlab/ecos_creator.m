function ecos_creator()
    solver_type = {'rome'};%'cvxsocp','rome','ecos','conelp', 'C'};
    
    test_problems;  % load the list of test problems
    
    try
        for j = 1:length(problems)
            for i = 1:length(solver_type),
                create_solver(problems{j}, solver_type{i});
            end
        end
    catch e
        cd ..   % return to top level directory
        e.message
    end 
end

function create_solver(directory, language)
    s = sprintf('Creating %s solver for %s.', upper(language), upper(directory));
    fprintf('%s\n',s);
    divider = s;
    for i = 1:length(divider)
        divider(i) = '=';
    end
    fprintf('%s\n',divider);
    
    cd(directory);
    
    try
        tic;
        switch lower(language)
            case 'cvxsocp'
                [status, result] = system(['../../src/scoop --cvxsocp ' directory '.prob']);
                copyfile([directory '/solver.m'], 'cvxsocp_solver.m', 'f');
            case 'ecos'
                [status, result] = system(['../../src/scoop --ecos ' directory '.prob']);
                copyfile([directory '/solver.m'], 'ecos_solver.m', 'f');
            case 'conelp'
                [status, result] = system(['../../src/scoop --conelp ' directory '.prob']);
                copyfile([directory '/solver.m'], 'conelp_solver.m', 'f');
            case 'rome'
                [status, result] = system(['../../src/scoop --rome ' directory '.prob']);
                copyfile([directory '/solver.m'], 'rome_solver.m', 'f');
            case 'c'
                [status, result] = system(['../../src/scoop --C ' directory '.prob']);
                cd(directory);
                try
                    evalc('makescoop');
                catch e
                    cd ..
                    throw(e);
                end
                cd ..
                copyfile([directory '/scooper.' mexext]);
            otherwise
                [status, result] = system(['../../src/scoop --ecos ' directory '.prob']);            
                copyfile([directory '/solver.m'], 'ecos_solver.m', 'f');
        end
        fprintf('  Took %f seconds to generate solver.\n\n', toc);
    catch e
        cd ..
        throw(e);
    end

    cd ..
end