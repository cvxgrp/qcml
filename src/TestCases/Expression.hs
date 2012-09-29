module TestCases.Expression (ExpressionTest(..),
  exprTests, exprTestCases) where
  import Expression.Expression
  import Rewriter.Atoms
  import Test.HUnit
  
  import Rewriter.ECOS
  
  -- test expressions
  data ExpressionTest = ExpressionTest { 
      expr :: CVXExpression,
      expectedVex :: Vexity,
      expectedSign :: Sign
    }
  
  expr1 = Leaf $ variable "x"
  expr2 = Leaf $ positiveParameter "1"
  expr3 = Leaf $ positiveParameter "8"
  expr4 = BinaryNode ecosPlus expr1 expr2 -- x + 1
  expr5 = UnaryNode ecosSquare expr1 -- x^2
  expr6 = BinaryNode ecosPlus expr5 expr2 -- x^2 + 1
  expr7 = BinaryNode ecosMinus expr5 expr2 -- x^2 - 1
  expr8 = UnaryNode ecosSquare expr6 -- square(x^2 + 1)
  expr9 = UnaryNode ecosSquare expr7 -- square(x^2 - 1)
  expr10 = UnaryNode ecosSqrt expr1 -- sqrt(x)
  expr11 = UnaryNode ecosInvPos expr10 -- 1/sqrt(x)
  expr12 = BinaryNode ecosMul expr3 expr1 -- 8*x
  
  exprTests = [ExpressionTest expr1 Affine Unknown,
    ExpressionTest expr2 Affine Positive,
    ExpressionTest expr12 Affine Unknown,
    ExpressionTest expr4 Affine Unknown,
    ExpressionTest expr5 Convex Positive,
    ExpressionTest expr8 Convex Positive,
    ExpressionTest expr9 Nonconvex Positive,
    ExpressionTest expr10 Concave Positive,
    ExpressionTest expr11 Convex Positive]
  
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
