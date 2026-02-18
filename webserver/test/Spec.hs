{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Exit (exitFailure)
import System.Timeout (timeout)

import Server (startServer)

main :: IO ()
main = do
  (port, stop) <- startServer "0"
  -- tiny delay to ensure accept loop is live
  _ <- timeout 200_000 (pure ())  -- no-op
  ok <- runAll port
  stop
  if ok then putStrLn "OK" else exitFailure

runAll :: Int -> IO Bool
runAll port = and <$> sequence
  [ testGetRoot port
  , testHeadNoBody port
  , testMissingHost400 port
  , testPipelineTwoRequests port
  , testConnectionClose port
  ]

-- ===== Helpers =====

assert :: String -> Bool -> IO Bool
assert name cond =
  if cond
    then putStrLn ("[PASS] " <> name) >> pure True
    else putStrLn ("[FAIL] " <> name) >> pure False

withConn :: Int -> (NS.Socket -> IO a) -> IO a
withConn port action = bracket (connectLocal port) NS.close action

connectLocal :: Int -> IO NS.Socket
connectLocal port = do
  s <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
  NS.connect s (NS.SockAddrInet (fromIntegral port) (NS.tupleToHostAddress (127,0,0,1)))
  pure s

recvUntilQuiet :: NS.Socket -> IO BS.ByteString
recvUntilQuiet s = go BS.empty
  where
    go acc = do
      m <- timeout 200_000 (NSB.recv s 4096)
      case m of
        Nothing -> pure acc
        Just bs
          | BS.null bs -> pure acc
          | otherwise  -> go (acc <> bs)

countSub :: BS.ByteString -> BS.ByteString -> Int
countSub needle hay
  | BS.null needle = 0
  | otherwise      = go 0 hay
  where
    go n bs =
      case B8.breakSubstring needle bs of
        (_pre, rest)
          | BS.null rest -> n
          | otherwise    -> go (n + 1) (BS.drop (BS.length needle) rest)

-- ===== Tests =====

testGetRoot :: Int -> IO Bool
testGetRoot port =
  withConn port $ \s -> do
    NSB.sendAll s "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
    out <- recvUntilQuiet s
    assert "GET / returns 200" ( "HTTP/1.1 200 OK" `BS.isPrefixOf` out && "ok\n" `BS.isInfixOf` out )

testHeadNoBody :: Int -> IO Bool
testHeadNoBody port =
  withConn port $ \s -> do
    NSB.sendAll s "HEAD / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
    out <- recvUntilQuiet s
    let parts = B8.breakSubstring "\r\n\r\n" out
        after = BS.drop 4 (snd parts)
    assert "HEAD / has no body" ("HTTP/1.1 200 OK" `BS.isPrefixOf` out && BS.null after)

testMissingHost400 :: Int -> IO Bool
testMissingHost400 port =
  withConn port $ \s -> do
    NSB.sendAll s "GET / HTTP/1.1\r\nConnection: close\r\n\r\n"
    out <- recvUntilQuiet s
    assert "HTTP/1.1 missing Host -> 400" ("HTTP/1.1 400" `BS.isPrefixOf` out)

testPipelineTwoRequests :: Int -> IO Bool
testPipelineTwoRequests port =
  withConn port $ \s -> do
    -- pipeline two requests; server should reply twice on same connection
    NSB.sendAll s $
      "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n" <>
      "GET /health HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
    out <- recvUntilQuiet s
    let n200 = countSub "HTTP/1.1 200 OK" out
    assert "pipelined GETs produce two 200 responses" (n200 == 2 && "healthy\n" `BS.isInfixOf` out)

testConnectionClose :: Int -> IO Bool
testConnectionClose port =
  withConn port $ \s -> do
    NSB.sendAll s "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
    _ <- recvUntilQuiet s
    -- If the server closed, a recv should return empty fairly quickly.
    m <- timeout 200_000 (NSB.recv s 1)
    let closed = case m of
          Nothing -> False
          Just bs -> BS.null bs
    assert "Connection: close closes socket" closed

