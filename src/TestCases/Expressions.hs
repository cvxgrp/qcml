module TestCases.Expressions (ExpressionTest(..),
  exprTests, exprTestCases) where
  import TestCases.TestExpressions
  import Expression.Expression
  import Rewriter.Atoms
  import Test.HUnit
    
  -- test expressions
  data ExpressionTest = ExpressionTest { 
      expr :: CVXExpression,
      expectedVex :: Vexity,
      expectedSign :: Sign
    }
  
  exprTests = [ExpressionTest expr1 Affine Unknown,
    ExpressionTest expr2 Affine Positive,
    ExpressionTest expr12 Affine Unknown,
    ExpressionTest expr4 Affine Unknown,
    ExpressionTest expr5 Convex Positive,
    ExpressionTest expr8 Convex Positive,
    ExpressionTest expr9 Nonconvex Positive,
    ExpressionTest expr10 Concave Positive,
    ExpressionTest expr11 Convex Positive,
    ExpressionTest expr13 Convex Positive]
  
  exprTestCases = map createExprTestCase exprTests
  
  createExprTestCase :: ExpressionTest -> Test
  createExprTestCase x =
    TestCase (do 
      assertBool ("Expected vexity is " 
        ++ (show $ expectedVex x) 
        ++ " but got " 
        ++ (show $ vexity (expr x))) 
        (expectedVex x == vexity (expr x))
      assertBool ("Expected sign is " 
        ++ (show $ expectedSign x) 
        ++ " but got " 
        ++ (show $ sign (expr x))) 
        (expectedSign x == sign (expr x))
      )
