module Main where
  import System.Console.GetOpt
  import Data.Char
  import System.IO
  import System.Environment
  import Parser.CVX
  
  -- need this for problem sense (should try to remove it somehow)
  import Expression.Expression  

  -- need this for code generators
  import CodeGenerator.CVX
  import CodeGenerator.CVXSOCP
  import CodeGenerator.ECOS
  import CodeGenerator.CGenerator
  
  import Data.Map as M
  
  ver = "0.0.1"
  
  data Flag
    = Version | CVX | CVXSOCP | Conelp | ECOS | C | Filename String
    deriving (Show, Eq)
  
  options :: [OptDescr Flag]
  options =
    [ Option ['v'] ["version"] (NoArg Version) "program version",
      Option [] ["cvx"] (NoArg CVX) "cvx output",
      Option [] ["cvxsocp"] (NoArg CVXSOCP) "cvx socp output",
      Option [] ["conelp"] (NoArg Conelp) "conelp matlab output",
      Option [] ["ecos"] (NoArg ECOS) "ecos matlab output",
      Option ['c'] ["C"] (NoArg C) "C code output (calls ECOS)"
    ]
  
  -- inp, outp :: Maybe String -> Flag
  -- outp = Output . fromMaybe "stdout"
  -- inp = Input . fromMaybe "stdin"
  
  programOpts :: [String] -> IO ([Flag], [String])
  programOpts argv =
    case getOpt RequireOrder options argv of
      (o, n, []) -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  
  header = "Usage: ProbToCVX [-v|--version] output probPath"
  

  runCVX :: Flag -> String -> IO ()
  runCVX flag input =
    let printFunc = case(flag) of
          CVX -> cvxgen
          CVXSOCP -> codegen
          Conelp -> codegenConelp
          ECOS -> codegenECOS
          C -> c_codegen -- c_header input
    in case (runParser cvxProg symbolTable "" input) of
        Left err -> do{ putStr "parse error at ";
                        print err }
        Right x  -> do{ 
          putStrLn $ printFunc x;
        }
        --case (sense x) of
        --  Minimize -> putStrLn (printFunc (rewrite x) 1)
        --  Maximize -> putStrLn (printFunc (rewrite x) (-1))
  
  main :: IO ()
  main = do
    args <- getArgs
    (flags, nonOpts)  <- programOpts args
    if (any (==Version) flags)
    then
      putStrLn ("ProbToCVX version " ++  ver)
    else
      case (flags,nonOpts) of
        ([opt], [filename]) -> 
          withFile filename ReadMode (\handle -> do
            contents <- hGetContents handle
            runCVX opt contents
            )
        _ -> error ("Invalid number of arguments\n\n" ++ usageInfo header options)
      