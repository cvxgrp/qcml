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
    let (m,n) = symbolSize x []
        newVar = VarId ("t" ++ (show count)) m n -- TODO: depends on size of leaf!
    in case x of
      Variable _ _ _ _ -> (0, prob x newVar [])
      otherwise -> (1, prob x newVar [])
  rewriteAndCount (Node x arguments) count =
    let (m,n) = symbolSize x (map size arguments)
        newVar = VarId ("t" ++ (show count)) m n-- TODO: depends on output of function
        (args, params) = splitAt (nargs x) arguments
        rewriters = take (nargs x) (map rewriteAndCount args)
        results = reduceRewriters rewriters count
        problems = map snd results
        total = foldl (+) 0 (map fst results)
        newProb = prob x newVar ((map objVar problems)++(map value params))
    in (total+1, foldl (<+) newProb problems)
  rewriteAndCount _ _ = (0,EmptyProblem)
  
  -- takes a list of rewriters and rewrites the arguments
  reduceRewriters :: [Int -> (Int,Problem)] -> Int -> [(Int,Problem)]
  reduceRewriters [f] n = [f (n+1)]
  reduceRewriters (f:fs) n = 
    let (count, problem) = f (n+1) 
    in (count, problem):(reduceRewriters fs (count+n))
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
  p1 <==> p2 = 
    let x = objVar p1
        y = objVar p2
        m1 = rows x -- assumes x,y have same length (they should, to get this far)
        n1 = cols x
        m2 = rows y
        n2 = cols y
        m = max m1 m2
        newMatrix = case (m1,m2) of
          (a,b) | a == b -> [[(x,Eye m "1"), (y, Eye m "-1")]]
          (1,b) -> [[(x, Ones m "1"), (y, Eye m "-1")]]
          (b,1) -> [[(x, Eye m "1"), (y, Ones m "-1")]]
          -- nonexhaustive search!!! XXX: we should never get here, but still....
        aMatrix = newMatrix ++ (matrixA p1) ++ (matrixA p2)
        bVector = (Ones m "0"):(vectorB p1) ++ (vectorB p2)
        cones = (conesK p1) ++ (conesK p2)
    in Problem Nothing aMatrix bVector cones
    
  -- comparing two problems / expressions via <=
  (<<=>) :: Problem -> Problem -> Problem
  p1 <<=> p2 = 
    let x = objVar p1
        y = objVar p2
        m1 = rows x
        n1 = cols x  -- should equal n2 == 1
        m2 = rows y
        n2 = cols y -- should equal n1 == 1
        m = max m1 m2
        n = max n1 n2
        t = VarId ((label x)++"LT"++(label y)) m n
        newMatrix = case (m1,m2) of
          (a,b) | a == b 
                -> [[(x,Eye m "1"), (t, Eye m "1"), (y, Eye m "-1")]]
          (1,b) -> [[(x, Ones m "1"), (t, Eye m "1"), (y, Eye m "-1")]]
          (b,1) -> [[(x, Eye m "1"), (t, Eye m "1"), (y, Ones m "-1")]]
          -- nonexhaustive search!!!
        aMatrix = newMatrix ++(matrixA p1) ++ (matrixA p2)
        bVector = (Ones m "0"):(vectorB p1) ++ (vectorB p2)
        cones = (conesK p1) ++ (conesK p2) ++ [SOCelem [t]]
    in Problem Nothing aMatrix bVector cones

    
  -- comparing two problems / expressions via >=
  (<>=>) :: Problem -> Problem -> Problem
  p1 <>=> p2 =  
    let x = objVar p1
        y = objVar p2
        m1 = rows x
        n1 = cols x  -- should equal size $ objVar y
        m2 = rows y
        n2 = cols y
        m = max m1 m2
        n = max n1 n2
        t = VarId ((label x)++"GT"++(label y)) m n
        newMatrix = case (m1,m2) of
          (a,b) | a == b 
                -> [[(x,Eye m "1"), (t, Eye m "-1"), (y, Eye m "-1")]]
          (1,b) -> [[(x, Ones m "1"), (t, Eye m "-1"), (y, Eye m "-1")]]
          (b,1) -> [[(x, Eye m "1"), (t, Eye m "-1"), (y, Ones m "-1")]]
          -- nonexhaustive search!!! XXX: we should never get here, but still....
        aMatrix = newMatrix ++(matrixA p1) ++ (matrixA p2)
        bVector = (Ones m "0"):(vectorB p1) ++ (vectorB p2)
        cones = (conesK p1) ++ (conesK p2) ++ [SOCelem [t]]
    in Problem Nothing aMatrix bVector cones
    
  -- returns the name of the parameter (if the expression is a paramter)
  value :: CVXExpression -> VarId
  value (Leaf (Parameter s f _ _ _)) = let (m,n) = f [] in VarId s m n
  value _ = VarId "NaN" 0 0 -- otherwise...?
  