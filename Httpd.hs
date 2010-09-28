-- A very simple web server
-- Copyright 2010 Geoff Hulette

module Httpd (runServer) where
import Control.Concurrent (forkIO)
import Control.Exception (try,IOException)
import Network
import Network.URI
import System.IO


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

runServer :: FilePath -> Integer -> IO ()
runServer root port = do
  sock <- listenOn (PortNumber (fromIntegral port))
  processRequests root sock
