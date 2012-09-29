module Rewriter.ECOS (rewrite) where
  import Rewriter.Atoms
  import Expression.Expression

  
  -- not the most efficient way of creating unique variable names 
  -- (requires counting nodes on left side before doing right side)
  
  -- really returns a problem (c, A, b, and K)
  -- TODO: how to keep parameter name instead of promoting during rewrite?
  -- TODO: provide rewrite rules for each atom
  
  -- a node has the ability to rewrite...
  -- nodeRewrite takes variable name, argument names, outputs a Problem
   
  rewrite :: CVXExpression -> Int -> Problem 
  rewrite (Leaf x) count = 
    let newVar = VarId ("t" ++ (show count)) 
    in prob x newVar []
  rewrite (BinaryNode x left right) count =
    let newVar = VarId ("t" ++ (show count))
        prob1 = rewrite left (count+1)
        prob2 = rewrite right (count+1+numNodes left)
    in (prob x newVar [obj prob1, obj prob2])
      <+ prob1 <+ prob2
  rewrite (UnaryNode x rest) count =
    let newVar = VarId ("t" ++ (show count))
        prob1 = rewrite rest (count+1)
    in (prob x newVar [obj prob1]) <+ prob1
  rewrite _ _ = Problem (VarId "t") [] [] []
  
  -- problem "merge" operator, retains the objective of LHS
  (<+) :: Problem -> Problem -> Problem
  x <+ y = Problem (obj x) ((matrixA x) ++ (matrixA y))
    ((vectorB x) ++ (vectorB y))
    ((conesK x) ++ (conesK y))
    
  -- returns the number of nodes in an expression
  numNodes :: CVXExpression -> Int
  numNodes (Leaf _) = 1
  numNodes (BinaryNode _ left right) 
    = 1 + numNodes left + numNodes right
  numNodes (UnaryNode _ rest) = 1 + numNodes rest
  numNodes _ = 0
