-- This is just an interpreter to help debug our parser and rewriter
-- You can't actually call a solver from this, since the entire code is
-- symbolic except for literal dimensions
module Main where
  import Data.Char
  import System.IO
  import Parser.CVX
  
  main :: IO ()
  main = do
    putStr "cvxi> "
    hFlush stdout   -- to flush out the prompt
    line <- getLine
    if (map toUpper line) == "EXIT"
    then return ()
    else do
      putStrLn (reverseWords line)
      -- attempt to lookup the string on the line, if fails, call parser
      
      putStrLn $ show $ parse line
      -- call parser on the line
      -- if an assignment occurs, should store in parser's symbol table
        -- routine to lookup symbols should be exposed in symbol table
      
      -- parser should return an expression tree
        -- if using a different parser, make a call to the external parser?
        -- requires writing the string to a BSON object
      
      -- if the keyword contains `minimize', call the rewriter
      -- rewriter uses DCP rules
        -- if using a different rewriter, make a call to the rewriter
        -- requires writing the expression tree to a BSON object
        
      -- (XXX: can't do this unless parameters instantiated) invoke solver from rewritten result
        
      -- echo the result
      
      main  -- run main again
    
  reverseWords :: String -> String
  reverseWords = unwords . map reverse . words