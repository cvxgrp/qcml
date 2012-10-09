module Main where
  import Test.HUnit
  import TestCases.Expressions
  import TestCases.Problems
  
  listOfExpressionTests
    = zipWith (\x y -> TestLabel ("Test: " ++ (show $ expr x)) y) 
      exprTests exprTestCases
      
  tests = TestList (listOfExpressionTests)

  main :: IO ()
  main = do
    x <- runTestTT tests
    putStrLn $ show x