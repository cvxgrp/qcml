module Main where
  import System.Console.GetOpt
  import Data.Char
  import Data.List
  import System.IO
  import System.Environment
  import System.Directory
  import Control.Monad
  import Parser.CVX
  
  -- need this for problem sense (should try to remove it somehow)
  import Expression.Expression  

  -- need this for code generators
  import CodeGenerator.CVX
  import CodeGenerator.CVXSOCP
  import CodeGenerator.ECOS
  import CodeGenerator.CGenerator
  
  import qualified Data.Map as M

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
  
  header = "Usage: EFE [-v|--version] output probPath"

  createSolverDirectory :: String -> IO String
  createSolverDirectory path = do {
      putStrLn $ "Creating directory " ++ pathName ++ "/";
      createDirectoryIfMissing False pathName;
      return pathName;
    }
    where indices = elemIndices '.' path
          pathName | indices == [] = path
                   | otherwise = take (last indices) path 


  
  runCVX :: Flag -> FilePath -> String -> IO ()
  runCVX flag dirpath input =
    let writers = case(flag) of
          CVX -> [(cvxgen, "/solver.m")]
          CVXSOCP -> [(codegen, "/solver.m")]
          Conelp -> [(codegenConelp, "/solver.m")]
          ECOS -> [(codegenECOS, "/solver.m")]
          C -> [(cCodegen, "/solver.c"), 
                (cHeader input,"/solver.h")]
    in case (runParser cvxProg symbolTable "" input) of
        Left err -> do{ putStr "parse error at ";
                        print err }
        Right x  -> forM_ writers (\(f, path) -> do {
          putStrLn $ "Generating code for " ++ (dirpath ++ path);
          writeFile (dirpath ++ path) (f x)
          })

  main :: IO ()
  main = do
    args <- getArgs
    (flags, nonOpts)  <- programOpts args
    if (any (==Version) flags)
    then
      putStrLn ("ProbToCVX version " ++  ver)
    else
      case (flags,nonOpts) of
        ([opt], [filename]) -> do {
          putStrLn "Running ECOS front end.";
          putStrLn "=======================";
          newDir <- createSolverDirectory filename;
          withFile filename ReadMode (\handle -> do
            putStrLn $ "Reading problem " ++ filename
            contents <- hGetContents handle
            putStrLn "Parsing..."
            runCVX opt newDir contents
            );
        }
        _ -> error ("Invalid number of arguments\n\n" ++ usageInfo header options)
      