module Rewriter.ECOS (rewrite) where
  import Rewriter.Atoms
  import Expression.Expression

  -- can we do things like square(A*x == 3)? and that writes to square(x) s.t. A*x == 3?

  
  -- a node / symbol has the ability to rewrite
  -- symbolRewrite takes variable name, argument names, outputs a Problem
  rewrite :: CVXProblem -> Problem 
  rewrite x = case (vexity x) of
    Nonconvex -> EmptyProblem
    otherwise -> 
      let (varcount, objProb) = rewriteAndCount (objective x) 0
          constraintSet = rewriteConstraintSet (constraints x) (varcount)
      in objProb <+ constraintSet
   
  -- rewrites the expression and returns a tuple that contains the count of 
  -- nodes and the problem
  --
  -- parser ensures that obj of prob1 and obj of prob2 is not Nothing
  rewriteAndCount :: CVXExpression -> Int -> (Int,Problem)
  rewriteAndCount (Leaf x) count = 
    let newVar = VarId ("t" ++ (show count)) -- TODO: depends on size of leaf!
        (m,n) = size x
    in case x of
      Variable _ _ _ _ -> (0, prob x (newVar m n) [])
      otherwise -> (1, prob x (newVar m n) [])
  rewriteAndCount (Node x arguments) count =
    let newVar = VarId ("t" ++ (show count)) -- TODO: depends on output of function
        (args, params) = splitAt (nargs x) arguments
        rewriters = take (nargs x) (map rewriteAndCount args)
        results = reduceRewriters rewriters count
        problems = map snd results
        total = foldl (+) 1 (map fst results)
        newProb = prob x (newVar 1 1) ((map objVar problems)++(map value params))
    in (total+1, foldl (<+) newProb problems)
  rewriteAndCount _ _ = (0,EmptyProblem)
  
  -- takes a list of rewriters and rewrites the arguments
  reduceRewriters :: [Int -> (Int,Problem)] -> Int -> [(Int,Problem)]
  reduceRewriters [f] n = [f (n+1)]
  reduceRewriters (f:fs) n = 
    let (count, problem) = f (n+1) 
    in (count, problem):(reduceRewriters fs (count+n+1))
  reduceRewriters _ _ = [(0,EmptyProblem)]
  
  
  -- problem "merge" operator, retains the objective of LHS
  (<+) :: Problem -> Problem -> Problem
  x <+ y = 
    let objective = obj x
        aMatrix = (matrixA x) ++ (matrixA y)
        bVector = (vectorB x) ++ (vectorB y)
        cones = (conesK x) ++ (conesK y)
    in Problem objective aMatrix bVector cones
  
  -- rewrites the constraints
  rewriteConstraintSet :: [CVXConstraint]->Int->Problem
  rewriteConstraintSet [] _ = Problem Nothing [] [] []
  rewriteConstraintSet (c:cs) count = 
    let (leftCount, leftProb) = rewriteAndCount (lhs c) count
        (rightCount, rightProb) = rewriteAndCount (rhs c) (count+leftCount)
        newCount = count + rightCount + leftCount
        leftoverConstraints = rewriteConstraintSet cs newCount
    in case (c) of
      (Eq _ _) -> (leftProb <==> rightProb) <+ leftoverConstraints
      (Leq _ _) -> (leftProb <<=> rightProb) <+ leftoverConstraints
      (Geq _ _) -> (leftProb <>=> rightProb) <+ leftoverConstraints

  -- equating two problems / expressions
  (<==>) :: Problem -> Problem -> Problem
  x <==> y = 
    let aMatrix = [[(objVar x,"1"), (objVar y, "-1")]]
          ++ (matrixA x) 
          ++ (matrixA y)
        bVector = "0":(vectorB x) ++ (vectorB y)
        cones = (conesK x) ++ (conesK y)
    in Problem Nothing aMatrix bVector cones
    
  -- comparing two problems / expressions via <=
  (<<=>) :: Problem -> Problem -> Problem
  x <<=> y = 
    let t = VarId ((objLabel x)++"LT"++(objLabel y)) 1 1
        aMatrix = [[(objVar x,"1"), (t,"1"), (objVar y, "-1")]]
          ++(matrixA x) ++ (matrixA y)
        bVector = "0":(vectorB x) ++ (vectorB y)
        cones = (conesK x) ++ (conesK y) ++ [SOC [t]]
    in Problem Nothing aMatrix bVector cones

    
  -- comparing two problems / expressions via >=
  (<>=>) :: Problem -> Problem -> Problem
  x <>=> y =  
    let t = VarId ((objLabel x)++"GT"++(objLabel y)) 1 1
        aMatrix = [[(objVar x,"1"), (t,"-1"), (objVar y, "-1")]]
          ++(matrixA x) ++ (matrixA y)
        bVector = "0":(vectorB x) ++ (vectorB y)
        cones = (conesK x) ++ (conesK y) ++ [SOC [t]]
    in Problem Nothing aMatrix bVector cones
    
  -- returns the name of the parameter (if the expression is a paramter)
  value :: CVXExpression -> VarId
  value (Leaf (Parameter s (m,n) _ _ _)) = VarId s m n
  value _ = VarId "NaN" 0 0 -- otherwise...?