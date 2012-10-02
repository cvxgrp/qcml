module Rewriter.ECOS (rewrite) where
  import Rewriter.Atoms
  import Expression.Expression
  import Problem.SOCP

  -- not the most efficient way of creating unique variable names 
  -- (requires counting nodes on left side before doing right side)
  
  -- really returns a problem (c, A, b, and K)
  
  -- a node / symbol has the ability to rewrite
  -- symbolRewrite takes variable name, argument names, outputs a Problem
  rewrite :: CVXProblem -> Problem 
  rewrite x = case (vexity x) of
    Nonconvex -> EmptyProblem
    otherwise -> let objProb = rewriteWithCount (objective x) 0
                     n = 5
      in objProb
   
  rewriteWithCount :: CVXExpression -> Int -> Problem 
  rewriteWithCount (Leaf x) count = 
    let newVar = VarId ("t" ++ (show count)) 
    in prob x newVar []
  rewriteWithCount (BinaryNode x left right) count =
    let newVar = VarId ("t" ++ (show count))
        prob1 = rewriteWithCount left (count+1)
        prob2 = rewriteWithCount right (count+1+numNodes left)
    in case(x) of
      ParamFunction _ _ _ _ _ _ -> 
        (prob x newVar [VarId (value left), obj prob2]) <+ prob2
      otherwise -> (prob x newVar [obj prob1, obj prob2]) <+ prob1 <+ prob2
  rewriteWithCount (UnaryNode x rest) count =
    let newVar = VarId ("t" ++ (show count))
        prob1 = rewriteWithCount rest (count+1)
    in (prob x newVar [obj prob1]) <+ prob1
  rewriteWithCount _ _ = EmptyProblem
  
  -- problem "merge" operator, retains the objective of LHS
  (<+) :: Problem -> Problem -> Problem
  x <+ y = Problem (obj x) ((matrixA x) ++ (matrixA y))
    ((vectorB x) ++ (vectorB y))
    ((conesK x) ++ (conesK y))

  -- equating two problems / expressions
  (<==>) :: Problem -> Problem -> Problem
  x <==> y = Problem (VarId "0") 
    ([[(obj x,"1"), (obj y, "-1")]]++(matrixA x) ++ (matrixA y))
    ("0":(vectorB x) ++ (vectorB y))
    ((conesK x) ++ (conesK y))
    
  -- comparing two problems / expressions via <=
  (<<=>) :: Problem -> Problem -> Problem
  x <<=> y = let t = VarId ((label $ obj x)++"LT"++(label $ obj y))
    in Problem (VarId "0") 
      ([[(obj x,"1"), (t,"1"), (obj y, "-1")]]
        ++(matrixA x) ++ (matrixA y))
      ("0":(vectorB x) ++ (vectorB y))
      ((conesK x) ++ (conesK y) ++ [SOC1 [t]])
    
  -- comparing two problems / expressions via >=
  (<>=>) :: Problem -> Problem -> Problem
  x <>=> y =  let t = VarId ((label $ obj x)++"GT"++(label $ obj y))
    in Problem (VarId "0") 
      ([[(obj x,"1"), (t,"-1"), (obj y, "-1")]]
        ++(matrixA x) ++ (matrixA y))
      ("0":(vectorB x) ++ (vectorB y))
      ((conesK x) ++ (conesK y) ++ [SOC1 [t]])
      
  -- returns the number of nodes in an expression
  numNodes :: CVXExpression -> Int
  numNodes (Leaf _) = 1
  numNodes (BinaryNode _ left right) 
    = 1 + numNodes left + numNodes right
  numNodes (UnaryNode _ rest) = 1 + numNodes rest
  numNodes _ = 0

  -- returns the name of the parameter (if the expression is a paramter)
  value :: CVXExpression -> String
  value (Leaf (Parameter s _ _ _)) = s
  value _ = "NaN" -- otherwise...?