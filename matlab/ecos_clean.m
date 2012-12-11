test_problems;
for i = 1:length(problems)
    cd(problems{i});
    eval(['!rm -rf ' problems{i}]);
    !rm ecos_solver.m;
    !rm cvxsocp_solver.m;
    !rm conelp_solver.m
    !rm rome_solver.m
    !rm scooper.*
    cd ..
end

