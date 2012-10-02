module Problem.Problem(CVXProblem(..), module Problem.SOCP) where
  import Expression.Expression
  import Problem.SOCP
  
  -- wait, i don't actually need this....
  -- data CVXProblem = CVXProblem {
  --     objective: CVXExpression,
  --     constraints: [CVXConstraints]
  --   }