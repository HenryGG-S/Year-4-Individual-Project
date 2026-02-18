{-# LANGUAGE OverloadedStrings #-}
module Server (runServer) where

import Control.Concurrent (forkFinally)
import Control.Exception (bracket)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BB
import Network.Socket
import qualified Network.Socket.ByteString as NSB

import Http.Types
import Http.Parse
import Http.Response

runServer :: String -> IO ()
runServer port = withSocketsDo $ bracket (open port) close acceptLoop

open :: String -> IO Socket
open port = do
  addr:_ <- getAddrInfo
              (Just defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream })
              Nothing
              (Just port)
  s <- socket (addrFamily addr) Stream defaultProtocol
  setSocketOption s ReuseAddr 1
  bind s (addrAddress addr)
  listen s 1024
  putStrLn ("Listening on :" <> port)
  pure s

acceptLoop :: Socket -> IO ()
acceptLoop s = go
  where
    go = do
      (c, _) <- accept s
      _ <- forkFinally (handleConn c) (\_ -> close c)
      go

handleConn :: Socket -> IO ()
handleConn c = connLoop BS.empty
  where
    connLoop buf = do
      (mReq, rest) <- readOneRequest c buf
      case mReq of
        Nothing  -> pure () -- client closed / no more data
        Just req -> do
          if not (requireHost req)
            then sendBuilder c (responseFor badRequest "Missing Host\n" True Close)
            else do
              let (st, body) = route req
                  pref = connectionPref req
                  sendBody = rqMethod req /= HEAD
              sendBuilder c (responseFor st body sendBody pref)
              case pref of
                Close     -> pure ()
                KeepAlive -> connLoop rest

route :: Request -> (Status, BS.ByteString)
route req =
  case rqMethod req of
    GET  -> pathRoute (rqTarget req)
    HEAD -> pathRoute (rqTarget req)
 where
  pathRoute "/"       = (ok, "ok\n")
  pathRoute "/health" = (ok, "healthy\n")
  pathRoute _         = (notFound, "not found\n")

responseFor :: Status -> BS.ByteString -> Bool -> ConnectionPref -> BB.Builder
responseFor st body sendBody pref =
  let connHdr = case pref of
                  KeepAlive -> ("Connection","keep-alive")
                  Close     -> ("Connection","close")
  in mkResponse st [("Content-Type","text/plain; charset=utf-8"), connHdr] body sendBody

-- Incremental parse: return (Maybe Request, remaining buffer)
readOneRequest :: Socket -> BS.ByteString -> IO (Maybe Request, BS.ByteString)
readOneRequest c buf = step (A.parse requestP buf)
  where
    step (A.Done rest req) = pure (Just req, rest)
    step (A.Fail _ _ _)    = pure (Just (Request GET "/" "HTTP/1.1" []), BS.empty)  -- replaced below
    step (A.Partial k) = do
      chunk <- NSB.recv c 4096
      if BS.null chunk
        then pure (Nothing, BS.empty)
        else step (k chunk)

sendBuilder :: Socket -> BB.Builder -> IO ()
sendBuilder c b = go (BB.toLazyByteString b)
  where
    go lbs
      | LBS.null lbs = pure ()
      | otherwise = do
          let (x, xs) = LBS.splitAt 16384 lbs
          NSB.sendAll c (LBS.toStrict x)
          go xs

