module Main where
  import Test.HUnit
  import Parser.CVX
  import Parser.CVXDataTypes
  import Rewriter.ECOS
  
  -- test cases for the lexer
  s1 = "ABC"
  s2 = "minimizeA*x+b>"
  s3 = "minimize sum_square(A*x+b) + lambda*norm2(x) subjectTo B*x==d, x >= 0"
  s4 = "x=3; 3="
  s5 = "n=5 <"
  
  -- expected outputs for the lexer
  t1 = [Identifier "ABC"]
  t2 = [Identifier "minimizeA", Multiply, Identifier "x", 
    Plus, Identifier "b>"]
  t3 = [Minimize, Identifier "sum_square", LeftParen, Identifier "A",
    Multiply, Identifier "x", Plus, Identifier "b", RightParen, Plus,
    Identifier "lambda", Multiply, Identifier"norm2", LeftParen,
    Identifier "x", RightParen, SubjectTo, Identifier "B", Multiply,
    Identifier "x", Equals, Identifier "d", Comma, Identifier "x",
    GreaterThanEquals, Literal 0]
  t4 = [Identifier "x", Assign, Identifier "3;", Literal 3, Assign]
  t5 = [Identifier "n", Assign, Literal 5, Identifier "<"]

  a = case cvxLex s1 of
    [Identifier "ABC"] -> True
    _ -> False   
  b = case cvxLex s2 of
    [Identifier "minimizeA", Multiply, Identifier "x", 
      Plus, Identifier "b>"] -> True
    _ -> False
  c = case cvxLex s3 of
    [Minimize, Identifier "sum_square", LeftParen, Identifier "A",
      Multiply, Identifier "x", Plus, Identifier "b", RightParen, Plus,
      Identifier "lambda", Multiply, Identifier"norm2", LeftParen,
      Identifier "x", RightParen, SubjectTo, Identifier "B", Multiply,
      Identifier "x", Equals, Identifier "d", Comma, Identifier "x",
      GreaterThanEquals, Literal 0] -> True
    _ -> False
  d = case cvxLex s4 of
    [Identifier "x", Assign, Identifier "3;", Literal 3, Assign] -> True
    _ -> False
  e = case cvxLex s5 of
    [Identifier "n", Assign, Literal 5, Identifier "<"] -> True
    _ -> False
     
  test1 = TestCase (do
      assertBool ("Expected " ++show t1++ " but got " ++ (show $ cvxLex s1)) a
    )
  test2 = TestCase (do
      assertBool ("Expected " ++show t2++ " but got " ++ (show $ cvxLex s2)) b
    )
  test3 = TestCase (do
      assertBool ("Expected " ++show t3++ " but got " ++ (show $ cvxLex s3)) c
    )
  test4 = TestCase (do
      assertBool ("Expected " ++show t4++ " but got " ++ (show $ cvxLex s4)) d
    )
  test5 = TestCase (do
      assertBool ("Expected " ++show t5++ " but got " ++ (show $ cvxLex s5)) e
    )
  
  
  expr1 = [(Const 2.4)] --[(Var "x"), (Const 2.4), (BinaryOp "*")]
  expr2 = [(Const 2.4), (Var "x"), (BinaryOp "mul"), (Const (-0.1)), (BinaryOp "plus"), (UnaryOp "square"), (UnaryOp "square")]
  
  test6 = TestCase(do
    assertBool ("Expected true but got " ++ (show expr1)) (isDCP expr1)
    )
  
  test7 = TestCase(do
    assertBool ("Expected true but got " ++ (show expr2)) (isDCP expr2)
    )
  
  tests = TestList [
    TestLabel ("Test: " ++ s1) test1,
    TestLabel ("Test: " ++ s2) test2,
    TestLabel ("Test: " ++ s3) test3,  
    TestLabel ("Test: " ++ s4) test4,  
    TestLabel ("Test: " ++ s5) test5,   
    TestLabel ("Test: DCP") test6,
    TestLabel ("Test: DCP") test7
    ]

  main :: IO ()
  main = do
    x <- runTestTT tests
    putStrLn $ show x