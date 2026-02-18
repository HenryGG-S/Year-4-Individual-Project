{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkFinally)
import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Network.Socket
import qualified Network.Socket.ByteString as NSB

main :: IO ()
main = withSocketsDo $ bracket open close loop
  where
    open = do
      addr:_ <- getAddrInfo (Just defaultHints{addrFlags=[AI_PASSIVE], addrSocketType=Stream})
                            Nothing (Just "8080")
      s <- socket (addrFamily addr) Stream defaultProtocol
      setSocketOption s ReuseAddr 1
      bind s (addrAddress addr)
      listen s 1024
      pure s

    loop s = forever $ do
      (c, _) <- accept s
      _ <- forkFinally (handleConn c) (\_ -> close c)
      pure ()

handleConn :: Socket -> IO ()
handleConn c = do
  req <- recvUntilDoubleCRLF c 8192
  -- TODO: parse request-line + headers from req
  let body = "ok\n"
      resp = BS.concat
        [ "HTTP/1.1 200 OK\r\n"
        , "Content-Length: ", B8.pack (show (BS.length body)), "\r\n"
        , "Connection: close\r\n"
        , "\r\n"
        , body
        ]
  NSB.sendAll c resp

recvUntilDoubleCRLF :: Socket -> Int -> IO BS.ByteString
recvUntilDoubleCRLF c maxBytes = go BS.empty
  where
    go acc
      | BS.length acc >= maxBytes = pure acc
      | "\r\n\r\n" `BS.isInfixOf` acc = pure acc
      | otherwise = do
          chunk <- NSB.recv c 4096
          if BS.null chunk then pure acc else go (acc <> chunk)

forever :: IO a -> IO b
forever act = act >> forever act

