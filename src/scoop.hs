module Main where
  import System.Console.GetOpt
  --import Data.Char
  import Data.List(elemIndices)
  import System.IO
  import System.Environment(getArgs)
  --import Control.Monad
  import Parser.SCOOP
  
  -- need this for rows + cols
  -- import Expression.Expression  

  --import CodeGenerator.Common(Codegen(problem), getVariableRows)
  --import CodeGenerator.CVX
  
  import qualified Data.Map as M

  ver = "0.0.1"
  
  data Flag
    = Verbose | Version
    deriving (Show, Eq)

  options :: [OptDescr Flag]
  options =
    [ 
      Option [] ["version"] (NoArg Version) "program version",
      Option ['v'] ["verbose"] (NoArg Verbose) "verbose output"
    ]
  
  -- inp, outp :: Maybe String -> Flag
  -- outp = Output . fromMaybe "stdout"
  -- inp = Input . fromMaybe "stdin"
  
  programOpts :: [String] -> IO ([Flag], [String])
  programOpts argv =
    case getOpt RequireOrder options argv of
      (o, n, []) -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  
  header = "Usage: SCOOP [--version] [-v|--verbose] probPath"

  getNewFilename :: String -> IO String
  getNewFilename path = return fileName
    where indices = elemIndices '.' path
          fileName | indices == [] = path ++ ".scoop"
                   | otherwise = take (last indices) path ++ ".scoop"

  -- belongs in codegen common or something....
  --formatStats :: Codegen -> String
  --formatStats x = unlines $
  --  ["Problem statistic summary",
  --   "=========================",
  --   "  original problem",
  --   "    " ++ show nump ++ " parameters (in " ++ show lenp ++ " symbols)",
  --   "    " ++ show numv ++ " variables (in " ++ show lenv ++ " vectors)",
  --   "",
  --   "  transformed problem",
  --   "    " ++ show nump ++ " parameters (in " ++ show lenp ++ " symbols)",
  --   "    " ++ show numtv ++ " variables (in " ++ show lentv ++ " vectors)"]
  --  where params = paramlist x
  --        vars = varlist x
  --        (lenp, lenv) = (length params, length vars)
  --        sizes x = (rows x)*(cols x)
  --        (nump, numv) = (foldl (+) 0 (map sizes params), foldl (+) 0 (map sizes vars))
  --        tvsizes = getVariableRows (problem x)
  --        lentv = length tvsizes
  --        numtv = foldl (+) 0 tvsizes


  --printProblemStatistics :: Codegen -> IO ()
  --printProblemStatistics x = putStrLn (formatStats x)


  runSCOOP :: Bool -> FilePath -> String -> IO ()
  runSCOOP verb filename input =
    case (runParser cvxProg symbolTable "" input) of
        Left err -> do{ logStr True "parse error at ";
                        logPrint True err }
        Right x  -> do {
            logStrLn verb "Rewriting problem ...";
            logStrLn verb "";

            putStr x;

            -- logStrLn verb "";
            -- printProblemStatistics x;
          }
          -- TODO: print statistics for problem (done by looking at symbols in x)
          -- TODO: print statistics for *transformed* problem (done by look at "problem x")

  -- auxilary log functions
  logPrint :: Show a => Bool -> a -> IO ()
  logPrint True = hPrint stderr
  logPrint False = (\_ -> return ())

  logStr :: Bool -> String -> IO ()
  logStr True = hPutStr stderr
  logStr False = (\_ -> return ())

  logStrLn :: Bool -> String -> IO ()
  logStrLn True = hPutStrLn stderr
  logStrLn False = (\_ -> return ())


  mainProg :: Bool -> String -> IO ()
  mainProg verb filename = do
    logStrLn verb "";
    logStrLn verb "Running SCOOP.";
    logStrLn verb "==============";

    newFile <- getNewFilename filename;
    withFile filename ReadMode (\handle -> do
      logStrLn verb $ "Reading problem " ++ filename
      contents <- hGetContents handle

      logStrLn verb "Parsing ..."
      runSCOOP verb newFile contents
      );

  main :: IO ()
  main = do
    args <- getArgs
    (flags, nonOpts)  <- programOpts args
    if (any (==Version) flags)
    then
      putStrLn ("SCOOP version " ++  ver)
    else
      case (flags,nonOpts) of
        ([Verbose], [filename]) -> mainProg True filename
        ([], [filename]) -> mainProg False filename
        _ -> error ("Invalid number of arguments\n\n" ++ usageInfo header options)
      