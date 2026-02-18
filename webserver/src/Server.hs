{-# LANGUAGE OverloadedStrings #-}
module Server
  ( runServer
  , startServer      -- for tests
  ) where

import Control.Concurrent (forkFinally)
import Control.Exception (IOException, bracket, catch)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Timeout (timeout)

import Http.Types
import Http.Parse
import Http.Response
import Workloads (json1k, file50k, file1m)

-- ===== Public API =====

runServer :: String -> IO ()
runServer port =
  NS.withSocketsDo $ bracket (open port) NS.close acceptLoop

-- Start on a port (use "0" for ephemeral) and return (chosenPort, stopAction).
startServer :: String -> IO (Int, IO ())
startServer port = do
  sock <- open port
  chosen <- socketPort sock
  _ <- forkFinally (acceptLoop sock) (\_ -> pure ())
  let stop = NS.close sock >> pure ()
  pure (chosen, stop)

-- ===== Socket setup =====

open :: String -> IO NS.Socket
open port = do
  addr:_ <-
    NS.getAddrInfo
      (Just NS.defaultHints
        { NS.addrFlags      = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        })
      Nothing
      (Just port)

  s <- NS.socket (NS.addrFamily addr) NS.Stream NS.defaultProtocol
  NS.setSocketOption s NS.ReuseAddr 1
  NS.bind s (NS.addrAddress addr)
  NS.listen s 1024

  p <- socketPort s
  putStrLn ("Listening on :" <> show p)
  pure s

socketPort :: NS.Socket -> IO Int
socketPort s = do
  sa <- NS.getSocketName s
  case sa of
    NS.SockAddrInet p _      -> pure (fromIntegral p)
    NS.SockAddrInet6 p _ _ _ -> pure (fromIntegral p)
    _                        -> pure 0

acceptLoop :: NS.Socket -> IO ()
acceptLoop s = go `catch` handler
  where
    handler :: IOException -> IO ()
    handler _ = pure ()

    go = do
      (c, _peer) <- NS.accept s
      _ <- forkFinally (handleConn c) (\_ -> NS.close c)
      go

-- ===== Connection handling =====

handleConn :: NS.Socket -> IO ()
handleConn c = connLoop BS.empty
  where
    connLoop buf = do
      res <- readOneRequest c buf
      case res of
        Left e -> do
          let (st, msg) =
                case e of
                  TooLarge -> (requestHeaderFieldsTooLarge, "Header fields too large\n")
                  ParseErr -> (badRequest, "Bad Request\n")
                  TimedOut -> (badRequest, "Request Timeout\n")
          sendBuilder c (responseFor st ctText msg True Close [])
        Right (Nothing, _rest) ->
          pure ()
        Right (Just req, rest) -> do
          if not (requireHost req)
            then do
              sendBuilder c (responseFor badRequest ctText "Missing Host\n" True Close [])
              pure ()
            else if hasBody req
              then do
                sendBuilder c (responseFor badRequest ctText "Request bodies not supported\n" True Close [])
                pure ()
            else case rqMethod req of
              Other _ -> do
                sendBuilder c (responseFor methodNotAllowed ctText "Method Not Allowed\n" True Close
                                [("Allow","GET, HEAD")])
                pure ()
              _ -> do
                let (st, ct, body) = route req
                    pref            = connectionPref req
                    sendBody        = rqMethod req /= HEAD
                sendBuilder c (responseFor st ct body sendBody pref [])
                case pref of
                  Close     -> pure ()
                  KeepAlive -> connLoop rest

hasBody :: Request -> Bool
hasBody req =
  case headerLookup "Transfer-Encoding" (rqHeaders req) of
    Just _ -> True
    Nothing ->
      case headerLookup "Content-Length" (rqHeaders req) of
        Nothing -> False
        Just v  ->
          case B8.readInt (trimOWS v) of
            Just (n, _) -> n > 0
            Nothing     -> True
  where
    trimOWS = B8.dropWhile (\c -> c == ' ' || c == '\t')
           . B8.reverse . B8.dropWhile (\c -> c == ' ' || c == '\t') . B8.reverse

-- ===== Routing =====

ctText, ctJson, ctBin :: (BS.ByteString, BS.ByteString)
ctText = ("Content-Type", "text/plain; charset=utf-8")
ctJson = ("Content-Type", "application/json")
ctBin  = ("Content-Type", "application/octet-stream")

route :: Request -> (Status, (BS.ByteString, BS.ByteString), BS.ByteString)
route req =
  case rqTarget req of
    "/"        -> (ok, ctText, "ok\n")
    "/health"  -> (ok, ctText, "healthy\n")
    "/json"    -> (ok, ctJson, json1k)
    "/file50k" -> (ok, ctBin,  file50k)
    "/file1m"  -> (ok, ctBin,  file1m)
    _          -> (notFound, ctText, "not found\n")

-- ===== Response helpers =====

responseFor
  :: Status
  -> (BS.ByteString, BS.ByteString)                -- Content-Type
  -> BS.ByteString                                 -- body
  -> Bool                                          -- send body?
  -> ConnectionPref
  -> [(BS.ByteString, BS.ByteString)]              -- extra headers
  -> BB.Builder
responseFor st ct body sendBody pref extra =
  let connHdr =
        case pref of
          KeepAlive -> ("Connection", "keep-alive")
          Close     -> ("Connection", "close")
      hdrs = [ct] <> extra <> [connHdr]
  in mkResponse st hdrs body sendBody

-- ===== Incremental parse with bounds + timeout =====

data ReadError = ParseErr | TooLarge | TimedOut deriving (Eq, Show)

maxHeaderBytes :: Int
maxHeaderBytes = 8192

recvTimeoutMicros :: Int
recvTimeoutMicros = 5000000 -- 5s

readOneRequest :: NS.Socket -> BS.ByteString -> IO (Either ReadError (Maybe Request, BS.ByteString))
readOneRequest c buf0 = step buf0 (A.parse requestP buf0)
  where
    step buf (A.Done rest req) = pure (Right (Just req, rest))
    step _   (A.Fail _ _ _)    = pure (Left ParseErr)
    step buf (A.Partial k) =
      if BS.length buf > maxHeaderBytes
        then pure (Left TooLarge)
        else do
          mch <- timeout recvTimeoutMicros (NSB.recv c 4096)
          case mch of
            Nothing    -> pure (Left TimedOut)
            Just chunk ->
              if BS.null chunk
                then pure (Right (Nothing, BS.empty))
                else
                  let buf' = buf <> chunk
                  in step buf' (k chunk)

-- ===== Builder send =====

sendBuilder :: NS.Socket -> BB.Builder -> IO ()
sendBuilder c b = go (BB.toLazyByteString b)
  where
    go lbs
      | LBS.null lbs = pure ()
      | otherwise = do
          let (x, xs) = LBS.splitAt 16384 lbs
          NSB.sendAll c (LBS.toStrict x)
          go xs

