
module Main where
  import Data.Char
  import System.IO
  import System.Environment
  import Parser.CVX
  import Rewriter.ECOS
  
  main :: IO ()
  main = do
    args <- getArgs
    case (args) of
      (x:xs) -> withFile x ReadMode (\handle -> do
        contents <- hGetContents handle
        runLex cvxProb contents
        )
      _ -> putStrLn "Need an filename argument"
