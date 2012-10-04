module TestCases.Lexer (LexTest(..), lexerTests, lexerTestCases) where
  import Parser.CVX
  import OldParser.CVXDataTypes
  import Test.HUnit
  
  -- test cases for the lexer
  data LexTest = LexTest {
    line :: String,
    expectedTokens :: [Token]
  }
  
  lexerTests = [LexTest "ABC" [Identifier "ABC"],
    LexTest "minimizeA*x+b>" 
      [Identifier "minimizeA", Multiply, Identifier "x", 
      Plus, Identifier "b>"],
    LexTest "minimize sum_square(A*x+b) + lambda*norm2(x) subjectTo B*x==d, x >= 0" 
      [Minimize, Identifier "sum_square", LeftParen, Identifier "A",
      Multiply, Identifier "x", Plus, Identifier "b", RightParen, Plus,
      Identifier "lambda", Multiply, Identifier"norm2", LeftParen,
      Identifier "x", RightParen, SubjectTo, Identifier "B", Multiply,
      Identifier "x", Equals, Identifier "d", Comma, Identifier "x",
      GreaterThanEquals, Literal 0],
    LexTest "x=3; 3=" 
      [Identifier "x", Assign, Identifier "3;", Literal 3, Assign],
    LexTest "n=5 <"
      [Identifier "n", Assign, Literal 5, Identifier "<"]
    ]
  
  lexerTestCases = map createLexerTestCase lexerTests


  createLexerTestCase :: LexTest -> Test
  createLexerTestCase x =
    TestCase (do 
      assertBool ("Expected " 
        ++ (show $ expectedTokens x) 
        ++ " but got ")
        -- ++ (show $ cvxLex (line x))) 
        -- (expectedTokens x == cvxLex (line x))
        True
      )
  