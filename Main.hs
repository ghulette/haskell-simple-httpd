module Main (main) where
import Data.Monoid
import Httpd (runServer)
import System (getArgs)
import System.Console.GetOpt
import System.Posix

-- Default options

defaultOptions = Options
  { optPort = 8080
  , optRoot = "/tmp/httpd"
  }

-- Parsing command line options

data Options = Options
  { optPort :: Integer    -- Server port
  , optRoot :: FilePath   -- Root directory of the served file system
  } deriving (Eq,Show)
  
options :: [OptDescr (Options -> Options)]
options =
  [ Option ['p'] ["port"] 
    (ReqArg (\p opts -> opts { optPort = read p }) "N")
    "server port"
  , Option ['r'] ["root"] 
    (ReqArg (\f opts -> opts { optRoot = f }) "DIR")
    "root directory"
  ]

serverOpts :: [String] -> IO (Options, [String])
serverOpts argv = 
  case getOpt Permute options argv of
    (o,n,[]) -> do
      let foldf = appEndo . mconcat . (map Endo)
      return (foldf o defaultOptions, n)
    (_,_,errs) -> 
      ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: httpd [OPTION...]"


-- Main

main :: IO ()
main = do
  -- Parse command line options
  args <- getArgs
  (opts,_) <- serverOpts args
  let port = optPort opts
  let root = optRoot opts

  -- By default SIGPIPE exits the program, so ignore it.
  installHandler sigPIPE Ignore Nothing
  
  -- Run the server
  putStrLn $ "Starting server on port " ++ (show port) ++ " at " ++ root
  runServer root port
