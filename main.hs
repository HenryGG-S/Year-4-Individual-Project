{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.QSemN
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, finally, try)
import Control.Monad (forever, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Network.Socket
import qualified Network.Socket.ByteString as NBS
import System.Timeout (timeout)

data Config = Config
  { cfgPort          :: PortNumber
  , cfgMaxConns      :: Int
  , cfgReadTimeoutUs :: Int
  }

data Env = Env
  { envCfg  :: TVar Config
  , envLog  :: TChan LogMsg
  , envGate :: QSemN
  }

data LogMsg
  = Access !SockAddr !BS.ByteString !Int
  | Error  !SockAddr !BS.ByteString
  deriving Show

main :: IO ()
main = withSocketsDo $ do
  let cfg0 = Config { cfgPort = 8080, cfgMaxConns = 512, cfgReadTimeoutUs = 5_000_000 }

  cfgVar <- newTVarIO cfg0
  logCh  <- newTChanIO
  gate   <- newQSemN (cfgMaxConns cfg0)
  let env = Env cfgVar logCh gate

  void $ forkIO (loggerThread logCh)

  sock <- openListenSocket (cfgPort cfg0)
  putStrLn "listening on :8080"
  acceptLoop env sock

openListenSocket :: PortNumber -> IO Socket
openListenSocket port = do
  addr:_ <- getAddrInfo
              (Just defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream })
              Nothing
              (Just (show port))
  bracket (socket (addrFamily addr) Stream defaultProtocol) close $ \s -> do
    setSocketOption s ReuseAddr 1
    bind s (addrAddress addr)
    listen s 1024
    pure s

acceptLoop :: Env -> Socket -> IO ()
acceptLoop env sock = forever $ do
  (conn, peer) <- accept sock

  -- Backpressure: don't let active connections explode
  waitQSemN (envGate env) 1
  void . forkIO $
    finally (handleConn env peer conn) (signalQSemN (envGate env) 1)

handleConn :: Env -> SockAddr -> Socket -> IO ()
handleConn env peer conn =
  finally go (close conn)
 where
  go = do
    cfg <- readTVarIO (envCfg env)

    -- Very naive "request read": you will replace this with a real header reader.
    mbs <- timeout (cfgReadTimeoutUs cfg) (NBS.recv conn 4096)

    case mbs of
      Nothing -> atomically $ writeTChan (envLog env) (Error peer "timeout reading request")
      Just bs | BS.null bs ->
        pure ()
      Just bs -> do
        let respBody = "hello\n"
        NBS.sendAll conn (mkResp respBody)

        atomically $ writeTChan (envLog env) (Access peer (firstLine bs) 200)

firstLine :: BS.ByteString -> BS.ByteString
firstLine = BS.takeWhile (/= 10)  -- up to '\n' (still includes '\r' if present)

mkResp :: BS.ByteString -> BS.ByteString
mkResp body =
  BS.concat
    [ "HTTP/1.1 200 OK\r\n"
    , "Content-Length: ", B8.pack (show (BS.length body)), "\r\n"
    , "Connection: close\r\n"
    , "\r\n"
    , body
    ]

loggerThread :: TChan LogMsg -> IO ()
loggerThread ch = forever $ do
  msg <- atomically $ readTChan ch
  print msg
