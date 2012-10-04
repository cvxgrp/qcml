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
          constraintSet = rewriteConstraintSet (constraints x) varcount
      in objProb <+ constraintSet
   
  -- rewrites the expression and returns a tuple that contains the count of 
  -- nodes and the problem
  --
  -- parser ensures that obj of prob1 and obj of prob2 is not Nothing
  rewriteAndCount :: CVXExpression -> Int -> (Int,Problem)
  rewriteAndCount (Leaf x) count = 
    let newVar = VarId ("t" ++ (show count)) 
    in (1, prob x newVar [])
  
  rewriteAndCount (BinaryNode x left right) count =
    let newVar = VarId ("t" ++ (show count))
        (leftCount, prob1) = rewriteAndCount left (count+1)
        (rightCount, prob2) = rewriteAndCount right (count+1+leftCount)
        total = leftCount + rightCount
    in case(x) of
      ParamFunction _ _ _ _ _ _ -> 
        (total+1, (prob x newVar [VarId (value left), objVar prob2]) <+ prob2)
      otherwise -> 
        (total+1, (prob x newVar [objVar prob1, objVar prob2]) <+ prob1 <+ prob2)
        
  rewriteAndCount (UnaryNode x rest) count =
    let newVar = VarId ("t" ++ (show count))
        (total, prob1) = rewriteAndCount rest (count+1)
    in (total+1, (prob x newVar [objVar prob1]) <+ prob1)
    
  rewriteAndCount _ _ = (0,EmptyProblem)
  
  -- problem "merge" operator, retains the objective of LHS
  (<+) :: Problem -> Problem -> Problem
  x <+ y = 
    let objective = obj x
        aMatrix = (matrixA x) ++ (matrixA y)
        bVector = (vectorB x) ++ (vectorB y)
        cones = (conesK x) ++ (conesK y)
    in Problem objective aMatrix bVector cones
  
  rewriteConstraintSet :: [CVXConstraint]->Int->Problem
  rewriteConstraintSet [] _ = Problem Nothing [] [] []
  rewriteConstraintSet (c:cs) count = 
    let (leftCount, leftProb) = rewriteAndCount (lhs c) count
        (rightCount, rightProb) = rewriteAndCount (rhs c) (count+leftCount)
        leftoverConstraints = (rewriteConstraintSet cs rightCount)
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
    let t = VarId ((objLabel x)++"LT"++(objLabel y))
        aMatrix = [[(objVar x,"1"), (t,"1"), (objVar y, "-1")]]
          ++(matrixA x) ++ (matrixA y)
        bVector = "0":(vectorB x) ++ (vectorB y)
        cones = (conesK x) ++ (conesK y) ++ [SOC [t]]
    in Problem Nothing aMatrix bVector cones

    
  -- comparing two problems / expressions via >=
  (<>=>) :: Problem -> Problem -> Problem
  x <>=> y =  
    let t = VarId ((objLabel x)++"GT"++(objLabel y))
        aMatrix = [[(objVar x,"1"), (t,"-1"), (objVar y, "-1")]]
          ++(matrixA x) ++ (matrixA y)
        bVector = "0":(vectorB x) ++ (vectorB y)
        cones = (conesK x) ++ (conesK y) ++ [SOC [t]]
    in Problem Nothing aMatrix bVector cones
    
  -- returns the name of the parameter (if the expression is a paramter)
  value :: CVXExpression -> String
  value (Leaf (Parameter s _ _ _)) = s
  value _ = "NaN" -- otherwise...?