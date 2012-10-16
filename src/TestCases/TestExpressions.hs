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
  expr4 = Node ecosPlus [expr1, expr2] -- x + 1
  expr5 = Node ecosSquare [expr1] -- x^2
  expr6 = Node ecosPlus [expr5, expr2] -- x^2 + 1
  expr7 = Node ecosMinus [expr5, expr2] -- x^2 - 1
  expr8 = Node ecosSquare [expr6] -- square(x^2 + 1)
  expr9 = Node ecosSquare [expr7] -- square(x^2 - 1)
  expr10 = Node ecosSqrt [expr1] -- sqrt(x)
  expr11 = Node ecosInvPos [expr10] -- 1/sqrt(x)
  expr12 = Node ecosMul [expr3, expr1] -- 8*x
  expr13 = Node ecosSquare [expr4] -- square(x + 1)
  expr14 = Node ecosMinus [expr12, expr2] -- 8*x - 1
  expr15 = Node ecosSquare [expr14] -- square(8*x - 1)
  expr16 = Node ecosPlus [expr11, expr1] -- 1/sqrt(x) + x