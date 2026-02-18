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
  _ <- timeout 200000 (pure ())  -- no-op
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
  , testJson1k port
  , testFile50k port
  , testFile1m port
  , testHeadJsonNoBody port
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
      m <- timeout 200000 (NSB.recv s 4096)
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

-- Read until we have headers (\r\n\r\n). Returns (headersIncludingMarker, initialBodyBytes).
recvHeaders :: NS.Socket -> IO (Maybe (BS.ByteString, BS.ByteString))
recvHeaders s = go BS.empty
  where
    go acc = do
      m <- timeout 1000000 (NSB.recv s 4096)
      case m of
        Nothing -> pure Nothing
        Just bs
          | BS.null bs -> pure Nothing
          | otherwise  ->
              let acc' = acc <> bs
                  (pre, rest) = B8.breakSubstring "\r\n\r\n" acc'
              in if BS.null rest
                    then go acc'
                    else pure (Just (pre <> "\r\n\r\n", BS.drop 4 rest))

recvExactly :: NS.Socket -> Int -> IO (Maybe BS.ByteString)
recvExactly _ 0 = pure (Just BS.empty)
recvExactly s n = go n BS.empty
  where
    go k acc
      | k <= 0 = pure (Just acc)
      | otherwise = do
          m <- timeout 1000000 (NSB.recv s (min 65536 k))
          case m of
            Nothing -> pure Nothing
            Just bs
              | BS.null bs -> pure Nothing
              | otherwise  -> go (k - BS.length bs) (acc <> bs)

expectContentLength :: Int -> BS.ByteString -> Bool
expectContentLength n hdrs =
  let needle = "Content-Length: " <> B8.pack (show n) <> "\r\n"
  in needle `BS.isInfixOf` hdrs

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
    let (_pre, rest) = B8.breakSubstring "\r\n\r\n" out
        after = BS.drop 4 rest
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
    m <- timeout 200000 (NSB.recv s 1)
    let closed = case m of
          Nothing -> False
          Just bs -> BS.null bs
    assert "Connection: close closes socket" closed

testJson1k :: Int -> IO Bool
testJson1k port =
  withConn port $ \s -> do
    NSB.sendAll s "GET /json HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
    mh <- recvHeaders s
    case mh of
      Nothing -> assert "GET /json headers received" False
      Just (hdrs, rest0) -> do
        let okStatus = "HTTP/1.1 200 OK" `BS.isPrefixOf` hdrs
            okLen    = expectContentLength 1024 hdrs
        let need = 1024 - BS.length rest0
        mb <- if need <= 0 then pure (Just (BS.take 1024 rest0))
                           else do
                             mmore <- recvExactly s need
                             pure ((rest0 <>) <$> mmore)
        case mb of
          Nothing -> assert "GET /json reads full body" False
          Just body -> assert "GET /json is 1024 bytes" (okStatus && okLen && BS.length body == 1024)

testFile50k :: Int -> IO Bool
testFile50k port =
  withConn port $ \s -> do
    NSB.sendAll s "GET /file50k HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
    mh <- recvHeaders s
    case mh of
      Nothing -> assert "GET /file50k headers received" False
      Just (hdrs, rest0) -> do
        let okStatus = "HTTP/1.1 200 OK" `BS.isPrefixOf` hdrs
            okLen    = expectContentLength 51200 hdrs
        let total = 51200
            need  = total - BS.length rest0
        mb <- if need <= 0 then pure (Just (BS.take total rest0))
                           else do
                             mmore <- recvExactly s need
                             pure ((rest0 <>) <$> mmore)
        case mb of
          Nothing -> assert "GET /file50k reads full body" False
          Just body -> assert "GET /file50k is 50KiB" (okStatus && okLen && BS.length body == total)

testFile1m :: Int -> IO Bool
testFile1m port =
  withConn port $ \s -> do
    NSB.sendAll s "GET /file1m HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
    mh <- recvHeaders s
    case mh of
      Nothing -> assert "GET /file1m headers received" False
      Just (hdrs, rest0) -> do
        let okStatus = "HTTP/1.1 200 OK" `BS.isPrefixOf` hdrs
            okLen    = expectContentLength 1048576 hdrs
        let total = 1048576
            need  = total - BS.length rest0
        mb <- if need <= 0 then pure (Just (BS.take total rest0))
                           else do
                             mmore <- recvExactly s need
                             pure ((rest0 <>) <$> mmore)
        case mb of
          Nothing -> assert "GET /file1m reads full body" False
          Just body -> assert "GET /file1m is 1MiB" (okStatus && okLen && BS.length body == total)

testHeadJsonNoBody :: Int -> IO Bool
testHeadJsonNoBody port =
  withConn port $ \s -> do
    NSB.sendAll s "HEAD /json HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
    out <- recvUntilQuiet s
    let (_pre, rest) = B8.breakSubstring "\r\n\r\n" out
        after = BS.drop 4 rest
        okStatus = "HTTP/1.1 200 OK" `BS.isPrefixOf` out
        okLen    = expectContentLength 1024 out
    assert "HEAD /json has no body (but CL=1024)" (okStatus && okLen && BS.null after)

