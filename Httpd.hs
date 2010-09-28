-- A very simple web server
-- Copyright 2010 Geoff Hulette

import Control.Concurrent (forkIO)
import Control.Exception (try,IOException)
import Data.Monoid
import Network
import Network.URI
import System (getArgs)
import System.Console.GetOpt
import System.IO
import System.Posix


-- Server stuff

data HttpRequest = HttpRequest 
  { requestMethod :: String
  , requestURI :: URI
  , requestProtocol :: String
  } deriving (Eq,Show)

parseRequest :: String -> Maybe HttpRequest
parseRequest s = do
  let [method,uriStr,protocol] = (words . head . lines) s
  uri <- parseURIReference uriStr
  return (HttpRequest method uri protocol)

handleRequest :: FilePath -> Handle -> HostName -> PortNumber -> IO ()
handleRequest root h host port = do
  putStrLn $ "Handling request from " ++ host ++ ":" ++ (show port)
  reqStr <- hGetContents h
  case parseRequest reqStr of
    Just req -> do
      let file = root ++ (unEscapeString . show . requestURI $ req)
      result <- try $ readFile file
      case result :: Either IOException String of
        Left err -> do
          putStrLn $ "File not found " ++ file
          hPutStrLn h "HTTP/1.1 404 NOT FOUND\n"
        Right body -> do
          putStrLn $ "Serving " ++ file
          hPutStrLn h "HTTP/1.1 200 OK\n"
          hPutStrLn h body
    Nothing -> do
      putStrLn "Could not parse request:"
      putStrLn reqStr
  hClose h

processRequests :: FilePath -> Socket -> IO ()
processRequests root servsock = do
  (h,host,port) <- accept servsock
  forkIO $ handleRequest root h host port
  processRequests root servsock

runServer :: FilePath -> PortID -> IO ()
runServer root port = do
  sock <- listenOn port
  processRequests root sock


-- Parsing command line options

data Options = Options
  { optPort :: Integer
  , optRoot :: FilePath
  } deriving (Eq,Show)

defaultOptions = Options
  { optPort = 8080
  , optRoot = "/tmp/httpd"
  }
  
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
  args <- getArgs
  (opts,_) <- serverOpts args
  print opts
  let port = optPort opts
  let root = optRoot opts
  putStrLn $ "Starting server on port " ++ (show port) ++ " at " ++ root
  installHandler sigPIPE Ignore Nothing
  runServer root (PortNumber (fromIntegral port))
