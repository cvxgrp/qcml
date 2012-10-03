module Main where
  import Test.HUnit
  import TestCases.Lexer
  import TestCases.Expressions
  import TestCases.Problems
  
  listOfLexerTests 
    = zipWith (\x y -> TestLabel ("Test: " ++ (line x)) y) 
      lexerTests lexerTestCases
  listOfExpressionTests
    = zipWith (\x y -> TestLabel ("Test: " ++ (show $ expr x)) y) 
      exprTests exprTestCases
      
  tests = TestList (listOfLexerTests ++ listOfExpressionTests)

  main :: IO ()
  main = do
    x <- runTestTT tests
    putStrLn $ show x