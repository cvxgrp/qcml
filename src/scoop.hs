module Main where
  import System.Console.GetOpt
  import Data.Char
  import Data.List
  import System.IO
  import System.Environment
  import System.Directory
  import Control.Monad
  import Parser.SCOOP
  
  -- need this for rows + cols
  import Expression.Expression  
  
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

  createRewriteFile :: Flag -> String -> IO String
  createRewriteFile flag path = do {
      logStrLn flag $ "Creating file " ++ pathName ++ "/";
      createDirectoryIfMissing False pathName;
      return pathName;
    }
    where indices = elemIndices '.' path
          pathName | indices == [] = path
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


  --runCVX :: Int -> Flag -> FilePath -> String -> IO ()
  --runCVX seed flag dirpath input =
  --  let writers = case(flag) of
  --        CVX -> [(cvxgen, "/solver.m")]
  --        CVXSOCP -> [(codegen, "/solver.m")]
  --        Conelp -> [(codegenConelp, "/solver.m")]
  --        ECOS -> [(codegenECOS, "/solver.m")]
  --        C -> [(cCodegen, "/solver.c"), 
  --              (cHeader ver input,"/solver.h"),
  --              (makefile ecos_path, "/Makefile"),
  --              (cTestSolver, "/testsolver.c"),
  --              (mex, "/scooper.c"),
  --              (makescoop ecos_path, "/makescoop.m")]
  --  in case (runParser cvxProg symbolTable "" input) of
  --      Left err -> do{ putStr "parse error at ";
  --                      print err }
  --      Right x  -> do {
  --          forM_ writers (\(f, path) -> do {
  --            putStrLn $ "Generating code for " ++ (dirpath ++ path);
  --            writeFile (dirpath ++ path) (f x)
  --          });
  --          putStrLn "";
  --          printProblemStatistics x;
  --        }
          -- TODO: print statistics for problem (done by looking at symbols in x)
          -- TODO: print statistics for *transformed* problem (done by look at "problem x")

  logPrint :: Show a => Flag -> a -> IO ()
  logPrint Verbose = hPrint stderr
  logPrint _ = (\_ -> return ())

  logStr :: Flag -> String -> IO ()
  logStr Verbose = hPutStr stderr
  logStr _ = (\_ -> return ())

  logStrLn :: Flag -> String -> IO ()
  logStrLn Verbose = hPutStrLn stderr
  logStrLn _ = (\_ -> return ())

  main :: IO ()
  main = do
    args <- getArgs
    (flags, nonOpts)  <- programOpts args
    if (any (==Version) flags)
    then
      logStrLn Verbose ("SCOOP version " ++  ver)
    else
      case (flags,nonOpts) of
        ([opt], [filename]) -> do {
          logStrLn opt "";
          logStrLn opt "Running SCOOP.";
          logStrLn opt "==============";

          newDir <- createRewriteFile opt filename;
          withFile filename ReadMode (\handle -> do
            logStrLn opt $ "Reading problem " ++ filename
            contents <- hGetContents handle

            -- get RNG seed
            --utc <- getCurrentTime;
            --let seed = round (1000*(toRational (utctDayTime utc)));

            logStrLn opt "Parsing..."
            --runCVX seed opt newDir contents
            );
        }
        _ -> error ("Invalid number of arguments\n\n" ++ usageInfo header options)
      