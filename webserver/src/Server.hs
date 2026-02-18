{-# LANGUAGE OverloadedStrings #-}
module Server (runServer) where

import Control.Concurrent (forkFinally)
import Control.Exception (bracket)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import Http.Parse
import Http.Response
import Http.Types

runServer :: String -> IO ()
runServer port =
  NS.withSocketsDo $ bracket (open port) NS.close acceptLoop

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
  putStrLn ("Listening on :" <> port)
  pure s

acceptLoop :: NS.Socket -> IO ()
acceptLoop s = go
  where
    go = do
      (c, _peer) <- NS.accept s
      _ <- forkFinally (handleConn c) (\_ -> NS.close c)
      go

-- ===== Connection handling =====

handleConn :: NS.Socket -> IO ()
handleConn c = connLoop BS.empty
  where
    connLoop buf = do
      r <- readOneRequest c buf
      case r of
        Left ParseErr -> do
          sendBuilder c (responseFor badRequest "Bad Request\n" True Close)
          pure ()
        Right (Nothing, _rest) ->
          pure () -- client closed cleanly
        Right (Just req, rest) -> do
          if not (requireHost req)
            then do
              sendBuilder c (responseFor badRequest "Missing Host\n" True Close)
              pure ()
            else do
              let (st, body) = route req
                  pref       = connectionPref req
                  sendBody   = rqMethod req /= HEAD

              sendBuilder c (responseFor st body sendBody pref)

              case pref of
                Close     -> pure ()
                KeepAlive -> connLoop rest

-- ===== Routing (placeholder) =====

route :: Request -> (Status, BS.ByteString)
route req =
  case rqMethod req of
    GET  -> pathRoute (rqTarget req)
    HEAD -> pathRoute (rqTarget req)
  where
    pathRoute "/"       = (ok, "ok\n")
    pathRoute "/health" = (ok, "healthy\n")
    pathRoute _         = (notFound, "not found\n")

-- ===== Response helpers =====

responseFor :: Status -> BS.ByteString -> Bool -> ConnectionPref -> BB.Builder
responseFor st body sendBody pref =
  let connHdr =
        case pref of
          KeepAlive -> ("Connection", "keep-alive")
          Close     -> ("Connection", "close")
  in mkResponse st [("Content-Type", "text/plain; charset=utf-8"), connHdr] body sendBody

-- ===== Incremental parse with bounds =====

data ReadError = ParseErr deriving (Eq, Show)

maxHeaderBytes :: Int
maxHeaderBytes = 8192

-- Returns:
--   Left ParseErr                  -> malformed request / headers too large
--   Right (Nothing, _)             -> client closed
--   Right (Just req, remainingBuf) -> parsed one request, leftover bytes for next request
readOneRequest :: NS.Socket -> BS.ByteString -> IO (Either ReadError (Maybe Request, BS.ByteString))
readOneRequest c buf0 = step buf0 (A.parse requestP buf0)
  where
    step buf (A.Done rest req) = pure (Right (Just req, rest))
    step _   (A.Fail _ _ _)    = pure (Left ParseErr)
    step buf (A.Partial k) = do
      -- hard bound to avoid unbounded header growth
      if BS.length buf > maxHeaderBytes
        then pure (Left ParseErr)
        else do
          chunk <- NSB.recv c 4096
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

