module TestCases.TestExpressions (
  expr1, expr2, expr3, expr4, expr5,
  expr6, expr7, expr8, expr9, expr10,
  expr11, expr12, expr13, expr14, expr15,
  expr16) where
  import Expression.Expression
  import Rewriter.Atoms
  
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
  expr13 = UnaryNode ecosSquare expr4 -- square(x + 1)
  expr14 = BinaryNode ecosMinus expr12 expr2 -- 8*x - 1
  expr15 = UnaryNode ecosSquare expr14 -- square(8*x - 1)
  expr16 = BinaryNode ecosPlus expr11 expr1 -- 1/sqrt(x) + x