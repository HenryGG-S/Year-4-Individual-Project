{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

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

  -- Bodies + framing (general-purpose)
  , testPostEchoCL port
  , testPostEchoChunked port
  , testExpect100ContinueEcho port
  , testFramingConflictTECL port

  -- Filesystem-backed methods (general-purpose semantics)
  , testFsPutGetDeleteCL port
  , testFsPutChunked port
  , testFsPathTraversalRejected port
  , testDuplicateHost400 port
  , testInvalidHost400 port
  , testUnknownPath404 port
  , testKnownPathWrongMethod405 port
  , testDelete204NoContentLength port
  , testInvalidContentLengthRejected port
  , testMismatchedDuplicateContentLengthRejected port
  , testShortBodyRejected port
  , testMalformedChunkedRejected port
  , testHttp10ExpectIgnored port
  , testAbsoluteFormHealth port
  , testAbsoluteFormRoot port
  , testAuthorityFormNonConnect400 port
  , testConnectAuthorityNotImplemented port
  , testHostWithPortAccepted port
  , testBracketedIPv6HostAccepted port
  , testFsGetIncludesLastModified port
  , testFsIfModifiedSinceReturns304 port
  , testFsHeadIfModifiedSinceReturns304 port
  , testFsPutIfUnmodifiedSinceFails port
  , testFsPutIfUnmodifiedSinceSucceeds port
  , testFsDeleteIfUnmodifiedSinceFails port
  , testFsDeleteIfUnmodifiedSinceSucceeds port
  , testFsGetIncludesEtag port
  , testFsIfNoneMatchReturns304 port
  , testFsPutIfMatchFails port
  , testFsPutIfMatchSucceeds port
  , testFsDeleteIfMatchFails port
  , testFsDeleteIfMatchSucceeds port
  , testUnsupportedTransferEncoding501 port
  , testChunkedLongLine400 port
  , testFsGetRange206 port
  , testFsGetRange416 port
  , testFsHeadIgnoresRange port
  , testFsIfRangeDateExactMatch206 port
  , testFsIfRangeDateMismatch200 port
  , testFsIfRangeEtagMatch206 port
  ]

-- ===== Helpers =====
headerValue :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
headerValue name hdrs =
  let prefix = name <> ": "
      ls = map stripCR (B8.lines hdrs)
      stripCR = BS.takeWhile (/= 13)
  in case [BS.drop (BS.length prefix) l | l <- ls, prefix `BS.isPrefixOf` l] of
       v:_ -> Just v
       []  -> Nothing

hasHeader :: BS.ByteString -> BS.ByteString -> Bool
hasHeader name hdrs =
  (name <> ": ") `BS.isInfixOf` hdrs

bodyAfterHeaders :: BS.ByteString -> BS.ByteString
bodyAfterHeaders out =
  let (_pre, rest) = B8.breakSubstring "\r\n\r\n" out
  in BS.drop 4 rest

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
recvHeaders = recvHeadersWith BS.empty

-- Same, but begins with a provided buffer (useful when a previous read left extra bytes).
recvHeadersWith :: BS.ByteString -> NS.Socket -> IO (Maybe (BS.ByteString, BS.ByteString))
recvHeadersWith initial s = go initial
  where
    go acc =
      let (pre, rest) = B8.breakSubstring "\r\n\r\n" acc
      in if not (BS.null rest)
           then pure (Just (pre <> "\r\n\r\n", BS.drop 4 rest))
           else do
             m <- timeout 1000000 (NSB.recv s 4096)
             case m of
               Nothing -> pure Nothing
               Just bs
                 | BS.null bs -> pure Nothing
                 | otherwise  -> go (acc <> bs)

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

statusIs :: BS.ByteString -> BS.ByteString -> Bool
statusIs code hdrs = ("HTTP/1.1 " <> code) `BS.isPrefixOf` hdrs

-- ===== Existing tests (unchanged) =====

testGetRoot :: Int -> IO Bool
testGetRoot port =
  withConn port $ \s -> do
    NSB.sendAll s "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
    out <- recvUntilQuiet s
    assert "GET / returns 200"
      ("HTTP/1.1 200 OK" `BS.isPrefixOf` out && "ok\n" `BS.isInfixOf` out)

testHeadNoBody :: Int -> IO Bool
testHeadNoBody port =
  withConn port $ \s -> do
    NSB.sendAll s "HEAD / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
    out <- recvUntilQuiet s
    let (_pre, rest) = B8.breakSubstring "\r\n\r\n" out
        after = BS.drop 4 rest
    assert "HEAD / has no body"
      ("HTTP/1.1 200 OK" `BS.isPrefixOf` out && BS.null after)

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
    assert "pipelined GETs produce two 200 responses"
      (n200 == 2 && "healthy\n" `BS.isInfixOf` out)

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
        let okStatus = statusIs "200 OK" hdrs
            okLen    = expectContentLength 1024 hdrs
            need     = 1024 - BS.length rest0
        mb <- if need <= 0 then pure (Just (BS.take 1024 rest0))
                           else do mmore <- recvExactly s need
                                   pure ((rest0 <>) <$> mmore)
        case mb of
          Nothing   -> assert "GET /json reads full body" False
          Just body -> assert "GET /json is 1024 bytes" (okStatus && okLen && BS.length body == 1024)

testFile50k :: Int -> IO Bool
testFile50k port =
  withConn port $ \s -> do
    NSB.sendAll s "GET /file50k HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
    mh <- recvHeaders s
    case mh of
      Nothing -> assert "GET /file50k headers received" False
      Just (hdrs, rest0) -> do
        let okStatus = statusIs "200 OK" hdrs
            okLen    = expectContentLength 51200 hdrs
            total    = 51200
            need     = total - BS.length rest0
        mb <- if need <= 0 then pure (Just (BS.take total rest0))
                           else do mmore <- recvExactly s need
                                   pure ((rest0 <>) <$> mmore)
        case mb of
          Nothing   -> assert "GET /file50k reads full body" False
          Just body -> assert "GET /file50k is 50KiB" (okStatus && okLen && BS.length body == total)

testFile1m :: Int -> IO Bool
testFile1m port =
  withConn port $ \s -> do
    NSB.sendAll s "GET /file1m HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
    mh <- recvHeaders s
    case mh of
      Nothing -> assert "GET /file1m headers received" False
      Just (hdrs, rest0) -> do
        let okStatus = statusIs "200 OK" hdrs
            okLen    = expectContentLength 1048576 hdrs
            total    = 1048576
            need     = total - BS.length rest0
        mb <- if need <= 0 then pure (Just (BS.take total rest0))
                           else do mmore <- recvExactly s need
                                   pure ((rest0 <>) <$> mmore)
        case mb of
          Nothing   -> assert "GET /file1m reads full body" False
          Just body -> assert "GET /file1m is 1MiB" (okStatus && okLen && BS.length body == total)

testHeadJsonNoBody :: Int -> IO Bool
testHeadJsonNoBody port =
  withConn port $ \s -> do
    NSB.sendAll s "HEAD /json HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
    out <- recvUntilQuiet s
    let (_pre, rest) = B8.breakSubstring "\r\n\r\n" out
        after = BS.drop 4 rest
        okStatus = statusIs "200 OK" out
        okLen    = expectContentLength 1024 out
    assert "HEAD /json has no body (but CL=1024)" (okStatus && okLen && BS.null after)

-- ===== New: Bodies + framing =====

testPostEchoCL :: Int -> IO Bool
testPostEchoCL port =
  withConn port $ \s -> do
    NSB.sendAll s $
      "POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 4\r\nConnection: close\r\n\r\nping"
    mh <- recvHeaders s
    case mh of
      Nothing -> assert "POST /echo (CL) headers received" False
      Just (hdrs, rest0) -> do
        let okStatus = statusIs "200 OK" hdrs
            okLen    = expectContentLength 4 hdrs
            need     = 4 - BS.length rest0
        mb <- if need <= 0 then pure (Just (BS.take 4 rest0))
                           else do mmore <- recvExactly s need
                                   pure ((rest0 <>) <$> mmore)
        case mb of
          Nothing   -> assert "POST /echo (CL) reads full body" False
          Just body -> assert "POST /echo (CL) echoes body" (okStatus && okLen && body == "ping")

testPostEchoChunked :: Int -> IO Bool
testPostEchoChunked port =
  withConn port $ \s -> do
    NSB.sendAll s $
      "POST /echo HTTP/1.1\r\nHost: localhost\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n" <>
      "4\r\nping\r\n0\r\n\r\n"
    mh <- recvHeaders s
    case mh of
      Nothing -> assert "POST /echo (chunked) headers received" False
      Just (hdrs, rest0) -> do
        let okStatus = statusIs "200 OK" hdrs
            okLen    = expectContentLength 4 hdrs
            need     = 4 - BS.length rest0
        mb <- if need <= 0 then pure (Just (BS.take 4 rest0))
                           else do mmore <- recvExactly s need
                                   pure ((rest0 <>) <$> mmore)
        case mb of
          Nothing   -> assert "POST /echo (chunked) reads full body" False
          Just body -> assert "POST /echo (chunked) echoes body" (okStatus && okLen && body == "ping")

testExpect100ContinueEcho :: Int -> IO Bool
testExpect100ContinueEcho port =
  withConn port $ \s -> do
    -- Send headers only
    NSB.sendAll s $
      "POST /echo HTTP/1.1\r\nHost: localhost\r\nExpect: 100-continue\r\nContent-Length: 4\r\nConnection: close\r\n\r\n"

    mh1 <- recvHeaders s
    case mh1 of
      Nothing -> assert "Expect:100 got interim response" False
      Just (h1, rest1) -> do
        let ok100 = statusIs "100 Continue" h1
        if not ok100
          then assert "Expect:100 -> 100 Continue" False
          else do
            -- Now send body
            NSB.sendAll s "ping"
            mh2 <- recvHeadersWith rest1 s
            case mh2 of
              Nothing -> assert "Expect:100 got final response" False
              Just (h2, rest2) -> do
                let ok200 = statusIs "200 OK" h2 && expectContentLength 4 h2
                let need = 4 - BS.length rest2
                mb <- if need <= 0 then pure (Just (BS.take 4 rest2))
                                   else do mmore <- recvExactly s need
                                           pure ((rest2 <>) <$> mmore)
                case mb of
                  Nothing   -> assert "Expect:100 echo body read" False
                  Just body -> assert "Expect:100 echo works" (ok200 && body == "ping")

testFramingConflictTECL :: Int -> IO Bool
testFramingConflictTECL port =
  withConn port $ \s -> do
    -- Both TE and CL: must be rejected (smuggling-safe behaviour)
    NSB.sendAll s $
      "POST /echo HTTP/1.1\r\nHost: localhost\r\nTransfer-Encoding: chunked\r\nContent-Length: 4\r\nConnection: close\r\n\r\n" <>
      "0\r\n\r\n"
    out <- recvUntilQuiet s
    assert "TE+CL conflict rejected"
      ("HTTP/1.1 400" `BS.isPrefixOf` out)

-- ===== New: Filesystem-backed semantics =====

testFsPutGetDeleteCL :: Int -> IO Bool
testFsPutGetDeleteCL port = do
  let p = "/fs/spec_test.txt"

  -- Best-effort cleanup: ignore result
  _ <- withConn port $ \s -> do
    NSB.sendAll s ("DELETE " <> p <> " HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
    _ <- recvUntilQuiet s
    pure ()

  okPut <- withConn port $ \s -> do
    NSB.sendAll s ("PUT " <> p <> " HTTP/1.1\r\nHost: localhost\r\nContent-Length: 5\r\nConnection: close\r\n\r\nhello")
    out <- recvUntilQuiet s
    -- accept 201 Created or 204 No Content (depending on your implementation)
    pure (("HTTP/1.1 201" `BS.isPrefixOf` out) || ("HTTP/1.1 204" `BS.isPrefixOf` out))

  okGet <- withConn port $ \s -> do
    NSB.sendAll s ("GET " <> p <> " HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
    mh <- recvHeaders s
    case mh of
      Nothing -> pure False
      Just (hdrs, rest0) -> do
        let okStatus = statusIs "200 OK" hdrs && expectContentLength 5 hdrs
            need = 5 - BS.length rest0
        mb <- if need <= 0 then pure (Just (BS.take 5 rest0))
                           else do mmore <- recvExactly s need
                                   pure ((rest0 <>) <$> mmore)
        pure (okStatus && mb == Just "hello")

  okDel <- withConn port $ \s -> do
    NSB.sendAll s ("DELETE " <> p <> " HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
    out <- recvUntilQuiet s
    pure (("HTTP/1.1 204" `BS.isPrefixOf` out) || ("HTTP/1.1 200" `BS.isPrefixOf` out))

  ok404 <- withConn port $ \s -> do
    NSB.sendAll s ("GET " <> p <> " HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
    out <- recvUntilQuiet s
    pure ("HTTP/1.1 404" `BS.isPrefixOf` out)

  assert "FS PUT/GET/DELETE (CL)" (okPut && okGet && okDel && ok404)

testFsPutChunked :: Int -> IO Bool
testFsPutChunked port = do
  let p = "/fs/spec_chunked.txt"

  okPut <- withConn port $ \s -> do
    NSB.sendAll s $
      "PUT " <> p <> " HTTP/1.1\r\nHost: localhost\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n" <>
      "5\r\nhello\r\n0\r\n\r\n"
    out <- recvUntilQuiet s
    pure (("HTTP/1.1 201" `BS.isPrefixOf` out) || ("HTTP/1.1 204" `BS.isPrefixOf` out))

  okGet <- withConn port $ \s -> do
    NSB.sendAll s ("GET " <> p <> " HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
    mh <- recvHeaders s
    case mh of
      Nothing -> pure False
      Just (hdrs, rest0) -> do
        let okStatus = statusIs "200 OK" hdrs && expectContentLength 5 hdrs
            need = 5 - BS.length rest0
        mb <- if need <= 0 then pure (Just (BS.take 5 rest0))
                           else do mmore <- recvExactly s need
                                   pure ((rest0 <>) <$> mmore)
        pure (okStatus && mb == Just "hello")

  assert "FS PUT (chunked) then GET" (okPut && okGet)

testFsPathTraversalRejected :: Int -> IO Bool
testFsPathTraversalRejected port =
  withConn port $ \s -> do
    -- Attempt to escape fs_root; must not succeed (should not be 200 OK).
    NSB.sendAll s "GET /fs/../bench_files/json1k.json HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
    out <- recvUntilQuiet s
    assert "FS path traversal rejected" (not ("HTTP/1.1 200" `BS.isPrefixOf` out))

testDuplicateHost400 :: Int -> IO Bool
testDuplicateHost400 port =
  withConn port $ \s -> do
    NSB.sendAll s $
      "GET / HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Host: example.com\r\n" <>
      "Connection: close\r\n\r\n"
    out <- recvUntilQuiet s
    assert "duplicate Host -> 400"
      ("HTTP/1.1 400" `BS.isPrefixOf` out)

testInvalidHost400 :: Int -> IO Bool
testInvalidHost400 port =
  withConn port $ \s -> do
    NSB.sendAll s $
      "GET / HTTP/1.1\r\n" <>
      "Host: bad host\r\n" <>
      "Connection: close\r\n\r\n"
    out <- recvUntilQuiet s
    assert "invalid Host -> 400"
      ("HTTP/1.1 400" `BS.isPrefixOf` out)

testUnknownPath404 :: Int -> IO Bool
testUnknownPath404 port =
  withConn port $ \s -> do
    NSB.sendAll s "GET /definitely-missing HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
    out <- recvUntilQuiet s
    assert "unknown path GET -> 404"
      ("HTTP/1.1 404" `BS.isPrefixOf` out)

testKnownPathWrongMethod405 :: Int -> IO Bool
testKnownPathWrongMethod405 port =
  withConn port $ \s -> do
    NSB.sendAll s "POST /json HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
    out <- recvUntilQuiet s
    assert "known path wrong method -> 405 with Allow"
      (("HTTP/1.1 405" `BS.isPrefixOf` out) &&
       ("Allow: GET, HEAD\r\n" `BS.isInfixOf` out))

testDelete204NoContentLength :: Int -> IO Bool
testDelete204NoContentLength port = do
  let p = "/fs/spec_204.txt"

  _ <- withConn port $ \s -> do
    NSB.sendAll s $
      "PUT " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Content-Length: 4\r\n" <>
      "Connection: close\r\n\r\ntest"
    _ <- recvUntilQuiet s
    pure ()

  withConn port $ \s -> do
    NSB.sendAll s $
      "DELETE " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    out <- recvUntilQuiet s
    let after = bodyAfterHeaders out
    assert "204 has no Content-Length and no body"
      (("HTTP/1.1 204 No Content" `BS.isPrefixOf` out) &&
       not (hasHeader "Content-Length" out) &&
       BS.null after)

testInvalidContentLengthRejected :: Int -> IO Bool
testInvalidContentLengthRejected port =
  withConn port $ \s -> do
    NSB.sendAll s $
      "POST /echo HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Content-Length: 5abc\r\n" <>
      "Connection: close\r\n\r\n" <>
      "ping!"
    out <- recvUntilQuiet s
    assert "invalid Content-Length rejected"
      ("HTTP/1.1 400" `BS.isPrefixOf` out)

testMismatchedDuplicateContentLengthRejected :: Int -> IO Bool
testMismatchedDuplicateContentLengthRejected port =
  withConn port $ \s -> do
    NSB.sendAll s $
      "POST /echo HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Content-Length: 4\r\n" <>
      "Content-Length: 5\r\n" <>
      "Connection: close\r\n\r\n" <>
      "ping!"
    out <- recvUntilQuiet s
    assert "mismatched duplicate Content-Length rejected"
      ("HTTP/1.1 400" `BS.isPrefixOf` out)

testShortBodyRejected :: Int -> IO Bool
testShortBodyRejected port =
  withConn port $ \s -> do
    NSB.sendAll s $
      "POST /echo HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Content-Length: 5\r\n" <>
      "Connection: close\r\n\r\n" <>
      "ping"
    NS.shutdown s NS.ShutdownSend
    out <- recvUntilQuiet s
    assert "short fixed-length body rejected"
      ("HTTP/1.1 400" `BS.isPrefixOf` out)

testMalformedChunkedRejected :: Int -> IO Bool
testMalformedChunkedRejected port =
  withConn port $ \s -> do
    NSB.sendAll s $
      "POST /echo HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Transfer-Encoding: chunked\r\n" <>
      "Connection: close\r\n\r\n" <>
      "4\r\nping\r\nX\r\n"
    out <- recvUntilQuiet s
    assert "malformed chunked body rejected"
      ("HTTP/1.1 400" `BS.isPrefixOf` out)

testHttp10ExpectIgnored :: Int -> IO Bool
testHttp10ExpectIgnored port =
  withConn port $ \s -> do
    NSB.sendAll s $
      "POST /echo HTTP/1.0\r\n" <>
      "Host: localhost\r\n" <>
      "Expect: 100-continue\r\n" <>
      "Content-Length: 4\r\n" <>
      "Connection: close\r\n\r\n"

    m <- timeout 200000 (NSB.recv s 256)
    let noInterim100 =
          case m of
            Nothing -> True
            Just bs -> not ("100 Continue" `BS.isInfixOf` bs)

    if not noInterim100
      then assert "HTTP/1.0 Expect ignored" False
      else do
        NSB.sendAll s "ping"
        out <- recvUntilQuiet s
        assert "HTTP/1.0 Expect ignored and final response still works"
          (not ("100 Continue" `BS.isInfixOf` out) &&
           ("200 OK" `BS.isInfixOf` out) &&
           ("ping" `BS.isInfixOf` out))

testAbsoluteFormHealth :: Int -> IO Bool
testAbsoluteFormHealth port =
  withConn port $ \s -> do
    NSB.sendAll s $
      "GET http://localhost/health HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    out <- recvUntilQuiet s
    assert "absolute-form GET /health works"
      ("HTTP/1.1 200 OK" `BS.isPrefixOf` out && "healthy\n" `BS.isInfixOf` out)

testAbsoluteFormRoot :: Int -> IO Bool
testAbsoluteFormRoot port =
  withConn port $ \s -> do
    NSB.sendAll s $
      "GET http://localhost HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    out <- recvUntilQuiet s
    assert "absolute-form GET with path-empty maps to /"
      ("HTTP/1.1 200 OK" `BS.isPrefixOf` out && "ok\n" `BS.isInfixOf` out)

testAuthorityFormNonConnect400 :: Int -> IO Bool
testAuthorityFormNonConnect400 port =
  withConn port $ \s -> do
    NSB.sendAll s $
      "GET localhost:8080 HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    out <- recvUntilQuiet s
    assert "authority-form non-CONNECT -> 400"
      ("HTTP/1.1 400" `BS.isPrefixOf` out)

testConnectAuthorityNotImplemented :: Int -> IO Bool
testConnectAuthorityNotImplemented port =
  withConn port $ \s -> do
    NSB.sendAll s $
      "CONNECT localhost:443 HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    out <- recvUntilQuiet s
    assert "CONNECT authority-form -> 501"
      ("HTTP/1.1 501" `BS.isPrefixOf` out)

testHostWithPortAccepted :: Int -> IO Bool
testHostWithPortAccepted port =
  withConn port $ \s -> do
    NSB.sendAll s $
      "GET / HTTP/1.1\r\n" <>
      "Host: localhost:8080\r\n" <>
      "Connection: close\r\n\r\n"
    out <- recvUntilQuiet s
    assert "Host with explicit port accepted"
      ("HTTP/1.1 200 OK" `BS.isPrefixOf` out)

testBracketedIPv6HostAccepted :: Int -> IO Bool
testBracketedIPv6HostAccepted port =
  withConn port $ \s -> do
    NSB.sendAll s $
      "GET / HTTP/1.1\r\n" <>
      "Host: [::1]:8080\r\n" <>
      "Connection: close\r\n\r\n"
    out <- recvUntilQuiet s
    assert "bracketed IPv6 Host accepted"
      ("HTTP/1.1 200 OK" `BS.isPrefixOf` out)

testFsGetIncludesLastModified :: Int -> IO Bool
testFsGetIncludesLastModified port = do
  let p = "/fs/spec_cache.txt"

  _ <- withConn port $ \s -> do
    NSB.sendAll s $
      "PUT " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Content-Length: 5\r\n" <>
      "Connection: close\r\n\r\nhello"
    _ <- recvUntilQuiet s
    pure ()

  withConn port $ \s -> do
    NSB.sendAll s $
      "GET " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    mh <- recvHeaders s
    case mh of
      Nothing -> assert "GET /fs includes Last-Modified headers received" False
      Just (hdrs, rest0) -> do
        let okStatus = statusIs "200 OK" hdrs
            hasLM    = hasHeader "Last-Modified" hdrs
            need     = 5 - BS.length rest0
        mb <- if need <= 0 then pure (Just (BS.take 5 rest0))
                           else do mmore <- recvExactly s need
                                   pure ((rest0 <>) <$> mmore)
        case mb of
          Nothing   -> assert "GET /fs includes Last-Modified full body" False
          Just body -> assert "GET /fs includes Last-Modified"
            (okStatus && hasLM && body == "hello")

testFsIfModifiedSinceReturns304 :: Int -> IO Bool
testFsIfModifiedSinceReturns304 port = do
  let p = "/fs/spec_cache_304.txt"

  _ <- withConn port $ \s -> do
    NSB.sendAll s $
      "PUT " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Content-Length: 5\r\n" <>
      "Connection: close\r\n\r\nhello"
    _ <- recvUntilQuiet s
    pure ()

  mLastMod <- withConn port $ \s -> do
    NSB.sendAll s $
      "GET " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    mh <- recvHeaders s
    pure $ case mh of
      Nothing        -> Nothing
      Just (hdrs, _) -> headerValue "Last-Modified" hdrs

  case mLastMod of
    Nothing -> assert "GET /fs produced Last-Modified for conditional GET" False
    Just lm ->
      withConn port $ \s -> do
        NSB.sendAll s $
          "GET " <> p <> " HTTP/1.1\r\n" <>
          "Host: localhost\r\n" <>
          "If-Modified-Since: " <> lm <> "\r\n" <>
          "Connection: close\r\n\r\n"
        out <- recvUntilQuiet s
        let after = bodyAfterHeaders out
        assert "If-Modified-Since -> 304"
          (("HTTP/1.1 304 Not Modified" `BS.isPrefixOf` out) &&
           hasHeader "Last-Modified" out &&
           BS.null after)

testFsHeadIfModifiedSinceReturns304 :: Int -> IO Bool
testFsHeadIfModifiedSinceReturns304 port = do
  let p = "/fs/spec_cache_head_304.txt"

  _ <- withConn port $ \s -> do
    NSB.sendAll s $
      "PUT " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Content-Length: 5\r\n" <>
      "Connection: close\r\n\r\nhello"
    _ <- recvUntilQuiet s
    pure ()

  mLastMod <- withConn port $ \s -> do
    NSB.sendAll s $
      "GET " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    mh <- recvHeaders s
    pure $ case mh of
      Nothing        -> Nothing
      Just (hdrs, _) -> headerValue "Last-Modified" hdrs

  case mLastMod of
    Nothing -> assert "HEAD /fs conditional produced Last-Modified source value" False
    Just lm ->
      withConn port $ \s -> do
        NSB.sendAll s $
          "HEAD " <> p <> " HTTP/1.1\r\n" <>
          "Host: localhost\r\n" <>
          "If-Modified-Since: " <> lm <> "\r\n" <>
          "Connection: close\r\n\r\n"
        out <- recvUntilQuiet s
        let after = bodyAfterHeaders out
        assert "HEAD with If-Modified-Since -> 304 no body"
          (("HTTP/1.1 304 Not Modified" `BS.isPrefixOf` out) &&
           hasHeader "Last-Modified" out &&
           BS.null after)

testFsPutIfUnmodifiedSinceFails :: Int -> IO Bool
testFsPutIfUnmodifiedSinceFails port = do
  let p = "/fs/spec_put_precond_fail.txt"
      oldDate = "Wed, 01 Jan 2020 00:00:00 GMT"

  _ <- withConn port $ \s -> do
    NSB.sendAll s $
      "PUT " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Content-Length: 5\r\n" <>
      "Connection: close\r\n\r\nhello"
    _ <- recvUntilQuiet s
    pure ()

  out <- withConn port $ \s -> do
    NSB.sendAll s $
      "PUT " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "If-Unmodified-Since: " <> oldDate <> "\r\n" <>
      "Content-Length: 5\r\n" <>
      "Connection: close\r\n\r\nworld"
    recvUntilQuiet s

  bodyOk <- withConn port $ \s -> do
    NSB.sendAll s $
      "GET " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    out2 <- recvUntilQuiet s
    pure ("hello" `BS.isInfixOf` out2 && not ("world" `BS.isInfixOf` out2))

  assert "PUT If-Unmodified-Since failure -> 412 and no overwrite"
    (("HTTP/1.1 412 Precondition Failed" `BS.isPrefixOf` out) &&
     hasHeader "Last-Modified" out &&
     bodyOk)

testFsPutIfUnmodifiedSinceSucceeds :: Int -> IO Bool
testFsPutIfUnmodifiedSinceSucceeds port = do
  let p = "/fs/spec_put_precond_ok.txt"

  _ <- withConn port $ \s -> do
    NSB.sendAll s $
      "PUT " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Content-Length: 5\r\n" <>
      "Connection: close\r\n\r\nhello"
    _ <- recvUntilQuiet s
    pure ()

  mLastMod <- withConn port $ \s -> do
    NSB.sendAll s $
      "GET " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    mh <- recvHeaders s
    pure $ case mh of
      Nothing        -> Nothing
      Just (hdrs, _) -> headerValue "Last-Modified" hdrs

  case mLastMod of
    Nothing -> assert "PUT precondition success source Last-Modified available" False
    Just lm -> do
      out <- withConn port $ \s -> do
        NSB.sendAll s $
          "PUT " <> p <> " HTTP/1.1\r\n" <>
          "Host: localhost\r\n" <>
          "If-Unmodified-Since: " <> lm <> "\r\n" <>
          "Content-Length: 5\r\n" <>
          "Connection: close\r\n\r\nworld"
        recvUntilQuiet s

      bodyOk <- withConn port $ \s -> do
        NSB.sendAll s $
          "GET " <> p <> " HTTP/1.1\r\n" <>
          "Host: localhost\r\n" <>
          "Connection: close\r\n\r\n"
        out2 <- recvUntilQuiet s
        pure ("world" `BS.isInfixOf` out2)

      assert "PUT If-Unmodified-Since success updates resource"
        (("HTTP/1.1 204 No Content" `BS.isPrefixOf` out) && bodyOk)

testFsDeleteIfUnmodifiedSinceFails :: Int -> IO Bool
testFsDeleteIfUnmodifiedSinceFails port = do
  let p = "/fs/spec_delete_precond_fail.txt"
      oldDate = "Wed, 01 Jan 2020 00:00:00 GMT"

  _ <- withConn port $ \s -> do
    NSB.sendAll s $
      "PUT " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Content-Length: 5\r\n" <>
      "Connection: close\r\n\r\nhello"
    _ <- recvUntilQuiet s
    pure ()

  out <- withConn port $ \s -> do
    NSB.sendAll s $
      "DELETE " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "If-Unmodified-Since: " <> oldDate <> "\r\n" <>
      "Connection: close\r\n\r\n"
    recvUntilQuiet s

  stillThere <- withConn port $ \s -> do
    NSB.sendAll s $
      "GET " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    out2 <- recvUntilQuiet s
    pure ("HTTP/1.1 200 OK" `BS.isPrefixOf` out2 && "hello" `BS.isInfixOf` out2)

  assert "DELETE If-Unmodified-Since failure -> 412 and file remains"
    (("HTTP/1.1 412 Precondition Failed" `BS.isPrefixOf` out) &&
     hasHeader "Last-Modified" out &&
     stillThere)

testFsDeleteIfUnmodifiedSinceSucceeds :: Int -> IO Bool
testFsDeleteIfUnmodifiedSinceSucceeds port = do
  let p = "/fs/spec_delete_precond_ok.txt"

  _ <- withConn port $ \s -> do
    NSB.sendAll s $
      "PUT " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Content-Length: 5\r\n" <>
      "Connection: close\r\n\r\nhello"
    _ <- recvUntilQuiet s
    pure ()

  mLastMod <- withConn port $ \s -> do
    NSB.sendAll s $
      "GET " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    mh <- recvHeaders s
    pure $ case mh of
      Nothing        -> Nothing
      Just (hdrs, _) -> headerValue "Last-Modified" hdrs

  case mLastMod of
    Nothing -> assert "DELETE precondition success source Last-Modified available" False
    Just lm -> do
      out <- withConn port $ \s -> do
        NSB.sendAll s $
          "DELETE " <> p <> " HTTP/1.1\r\n" <>
          "Host: localhost\r\n" <>
          "If-Unmodified-Since: " <> lm <> "\r\n" <>
          "Connection: close\r\n\r\n"
        recvUntilQuiet s

      gone <- withConn port $ \s -> do
        NSB.sendAll s $
          "GET " <> p <> " HTTP/1.1\r\n" <>
          "Host: localhost\r\n" <>
          "Connection: close\r\n\r\n"
        out2 <- recvUntilQuiet s
        pure ("HTTP/1.1 404 Not Found" `BS.isPrefixOf` out2)

      assert "DELETE If-Unmodified-Since success removes resource"
        (("HTTP/1.1 204 No Content" `BS.isPrefixOf` out) && gone)

testFsGetIncludesEtag :: Int -> IO Bool
testFsGetIncludesEtag port = do
  let p = "/fs/spec_etag.txt"

  _ <- withConn port $ \s -> do
    NSB.sendAll s $
      "PUT " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Content-Length: 5\r\n" <>
      "Connection: close\r\n\r\nhello"
    _ <- recvUntilQuiet s
    pure ()

  withConn port $ \s -> do
    NSB.sendAll s $
      "GET " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    mh <- recvHeaders s
    case mh of
      Nothing -> assert "GET /fs includes ETag headers received" False
      Just (hdrs, rest0) -> do
        let okStatus = statusIs "200 OK" hdrs
            hasETag  = hasHeader "ETag" hdrs
            need     = 5 - BS.length rest0
        mb <- if need <= 0 then pure (Just (BS.take 5 rest0))
                           else do mmore <- recvExactly s need
                                   pure ((rest0 <>) <$> mmore)
        case mb of
          Nothing   -> assert "GET /fs includes ETag full body" False
          Just body -> assert "GET /fs includes ETag"
            (okStatus && hasETag && body == "hello")

testFsIfNoneMatchReturns304 :: Int -> IO Bool
testFsIfNoneMatchReturns304 port = do
  let p = "/fs/spec_etag_304.txt"

  _ <- withConn port $ \s -> do
    NSB.sendAll s $
      "PUT " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Content-Length: 5\r\n" <>
      "Connection: close\r\n\r\nhello"
    _ <- recvUntilQuiet s
    pure ()

  mETag <- withConn port $ \s -> do
    NSB.sendAll s $
      "GET " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    mh <- recvHeaders s
    pure $ case mh of
      Nothing        -> Nothing
      Just (hdrs, _) -> headerValue "ETag" hdrs

  case mETag of
    Nothing -> assert "GET /fs produced ETag for conditional GET" False
    Just et -> withConn port $ \s -> do
      NSB.sendAll s $
        "GET " <> p <> " HTTP/1.1\r\n" <>
        "Host: localhost\r\n" <>
        "If-None-Match: " <> et <> "\r\n" <>
        "Connection: close\r\n\r\n"
      out <- recvUntilQuiet s
      let after = bodyAfterHeaders out
      assert "If-None-Match -> 304"
        (("HTTP/1.1 304 Not Modified" `BS.isPrefixOf` out) &&
         hasHeader "ETag" out &&
         BS.null after)

testFsPutIfMatchFails :: Int -> IO Bool
testFsPutIfMatchFails port = do
  let p = "/fs/spec_ifmatch_put_fail.txt"

  _ <- withConn port $ \s -> do
    NSB.sendAll s $
      "PUT " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Content-Length: 5\r\n" <>
      "Connection: close\r\n\r\nhello"
    _ <- recvUntilQuiet s
    pure ()

  out <- withConn port $ \s -> do
    NSB.sendAll s $
      "PUT " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "If-Match: \"definitely-wrong\"\r\n" <>
      "Content-Length: 5\r\n" <>
      "Connection: close\r\n\r\nworld"
    recvUntilQuiet s

  bodyOk <- withConn port $ \s -> do
    NSB.sendAll s $
      "GET " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    out2 <- recvUntilQuiet s
    pure ("hello" `BS.isInfixOf` out2 && not ("world" `BS.isInfixOf` out2))

  assert "PUT If-Match failure -> 412 and no overwrite"
    (("HTTP/1.1 412 Precondition Failed" `BS.isPrefixOf` out) &&
     hasHeader "ETag" out &&
     bodyOk)

testFsPutIfMatchSucceeds :: Int -> IO Bool
testFsPutIfMatchSucceeds port = do
  let p = "/fs/spec_ifmatch_put_ok.txt"

  _ <- withConn port $ \s -> do
    NSB.sendAll s $
      "PUT " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Content-Length: 5\r\n" <>
      "Connection: close\r\n\r\nhello"
    _ <- recvUntilQuiet s
    pure ()

  mETag <- withConn port $ \s -> do
    NSB.sendAll s $
      "GET " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    mh <- recvHeaders s
    pure $ case mh of
      Nothing        -> Nothing
      Just (hdrs, _) -> headerValue "ETag" hdrs

  case mETag of
    Nothing -> assert "PUT If-Match success source ETag available" False
    Just et -> do
      out <- withConn port $ \s -> do
        NSB.sendAll s $
          "PUT " <> p <> " HTTP/1.1\r\n" <>
          "Host: localhost\r\n" <>
          "If-Match: " <> et <> "\r\n" <>
          "Content-Length: 5\r\n" <>
          "Connection: close\r\n\r\nworld"
        recvUntilQuiet s

      bodyOk <- withConn port $ \s -> do
        NSB.sendAll s $
          "GET " <> p <> " HTTP/1.1\r\n" <>
          "Host: localhost\r\n" <>
          "Connection: close\r\n\r\n"
        out2 <- recvUntilQuiet s
        pure ("world" `BS.isInfixOf` out2)

      assert "PUT If-Match success updates resource"
        (("HTTP/1.1 204 No Content" `BS.isPrefixOf` out) && bodyOk)

testFsDeleteIfMatchFails :: Int -> IO Bool
testFsDeleteIfMatchFails port = do
  let p = "/fs/spec_ifmatch_delete_fail.txt"

  _ <- withConn port $ \s -> do
    NSB.sendAll s $
      "PUT " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Content-Length: 5\r\n" <>
      "Connection: close\r\n\r\nhello"
    _ <- recvUntilQuiet s
    pure ()

  out <- withConn port $ \s -> do
    NSB.sendAll s $
      "DELETE " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "If-Match: \"definitely-wrong\"\r\n" <>
      "Connection: close\r\n\r\n"
    recvUntilQuiet s

  stillThere <- withConn port $ \s -> do
    NSB.sendAll s $
      "GET " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    out2 <- recvUntilQuiet s
    pure ("HTTP/1.1 200 OK" `BS.isPrefixOf` out2 && "hello" `BS.isInfixOf` out2)

  assert "DELETE If-Match failure -> 412 and file remains"
    (("HTTP/1.1 412 Precondition Failed" `BS.isPrefixOf` out) &&
     hasHeader "ETag" out &&
     stillThere)

testFsDeleteIfMatchSucceeds :: Int -> IO Bool
testFsDeleteIfMatchSucceeds port = do
  let p = "/fs/spec_ifmatch_delete_ok.txt"

  _ <- withConn port $ \s -> do
    NSB.sendAll s $
      "PUT " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Content-Length: 5\r\n" <>
      "Connection: close\r\n\r\nhello"
    _ <- recvUntilQuiet s
    pure ()

  mETag <- withConn port $ \s -> do
    NSB.sendAll s $
      "GET " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    mh <- recvHeaders s
    pure $ case mh of
      Nothing        -> Nothing
      Just (hdrs, _) -> headerValue "ETag" hdrs

  case mETag of
    Nothing -> assert "DELETE If-Match success source ETag available" False
    Just et -> do
      out <- withConn port $ \s -> do
        NSB.sendAll s $
          "DELETE " <> p <> " HTTP/1.1\r\n" <>
          "Host: localhost\r\n" <>
          "If-Match: " <> et <> "\r\n" <>
          "Connection: close\r\n\r\n"
        recvUntilQuiet s

      gone <- withConn port $ \s -> do
        NSB.sendAll s $
          "GET " <> p <> " HTTP/1.1\r\n" <>
          "Host: localhost\r\n" <>
          "Connection: close\r\n\r\n"
        out2 <- recvUntilQuiet s
        pure ("HTTP/1.1 404 Not Found" `BS.isPrefixOf` out2)

      assert "DELETE If-Match success removes resource"
        (("HTTP/1.1 204 No Content" `BS.isPrefixOf` out) && gone)

-- ===== range / If-Range / unsupported TE / bounded chunk-line tests =====

seedFsBody :: Int -> BS.ByteString -> BS.ByteString -> IO ()
seedFsBody port p body = do
  _ <- withConn port $ \s -> do
    let cl = B8.pack (show (BS.length body))
    NSB.sendAll s $
      "PUT " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Content-Length: " <> cl <> "\r\n" <>
      "Connection: close\r\n\r\n" <>
      body
    _ <- recvUntilQuiet s
    pure ()
  pure ()

fetchFsHeader :: Int -> BS.ByteString -> BS.ByteString -> IO (Maybe BS.ByteString)
fetchFsHeader port p name =
  withConn port $ \s -> do
    NSB.sendAll s $
      "GET " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Connection: close\r\n\r\n"
    mh <- recvHeaders s
    pure $ case mh of
      Nothing        -> Nothing
      Just (hdrs, _) -> headerValue name hdrs

testUnsupportedTransferEncoding501 :: Int -> IO Bool
testUnsupportedTransferEncoding501 port =
  withConn port $ \s -> do
    NSB.sendAll s $
      "POST /echo HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Transfer-Encoding: gzip\r\n" <>
      "Connection: close\r\n\r\n"
    out <- recvUntilQuiet s
    assert "unsupported Transfer-Encoding -> 501"
      ("HTTP/1.1 501 Not Implemented" `BS.isPrefixOf` out)

testChunkedLongLine400 :: Int -> IO Bool
testChunkedLongLine400 port =
  withConn port $ \s -> do
    let longExt = BS.replicate 9000 97 -- 'a'
    NSB.sendAll s $
      "POST /echo HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Transfer-Encoding: chunked\r\n" <>
      "Connection: close\r\n\r\n" <>
      "1;" <> longExt <> "\r\n" <>
      "a\r\n" <>
      "0\r\n\r\n"
    out <- recvUntilQuiet s
    assert "overlong chunk-size line rejected"
      ("HTTP/1.1 400" `BS.isPrefixOf` out)

testFsGetRange206 :: Int -> IO Bool
testFsGetRange206 port = do
  let p = "/fs/spec_range_206.txt"
  seedFsBody port p "hello"

  withConn port $ \s -> do
    NSB.sendAll s $
      "GET " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Range: bytes=1-3\r\n" <>
      "Connection: close\r\n\r\n"
    mh <- recvHeaders s
    case mh of
      Nothing -> assert "GET range 206 headers received" False
      Just (hdrs, rest0) -> do
        let okStatus = statusIs "206 Partial Content" hdrs
            okLen    = expectContentLength 3 hdrs
            okCR     = headerValue "Content-Range" hdrs == Just "bytes 1-3/5"
            need     = 3 - BS.length rest0
        mb <- if need <= 0 then pure (Just (BS.take 3 rest0))
                           else do
                             mmore <- recvExactly s need
                             pure ((rest0 <>) <$> mmore)
        case mb of
          Nothing   -> assert "GET range 206 reads full body" False
          Just body -> assert "GET range bytes=1-3 -> 206 + sliced body"
            (okStatus && okLen && okCR && body == "ell")

testFsGetRange416 :: Int -> IO Bool
testFsGetRange416 port = do
  let p = "/fs/spec_range_416.txt"
  seedFsBody port p "hello"

  withConn port $ \s -> do
    NSB.sendAll s $
      "GET " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Range: bytes=99-100\r\n" <>
      "Connection: close\r\n\r\n"
    out <- recvUntilQuiet s
    assert "GET unsatisfiable range -> 416 with bytes */N"
      (("HTTP/1.1 416 Range Not Satisfiable" `BS.isPrefixOf` out) &&
       ("Content-Range: bytes */5\r\n" `BS.isInfixOf` out))

testFsHeadIgnoresRange :: Int -> IO Bool
testFsHeadIgnoresRange port = do
  let p = "/fs/spec_head_range.txt"
  seedFsBody port p "hello"

  withConn port $ \s -> do
    NSB.sendAll s $
      "HEAD " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Range: bytes=0-0\r\n" <>
      "Connection: close\r\n\r\n"
    out <- recvUntilQuiet s
    let after = bodyAfterHeaders out
    assert "HEAD ignores Range and returns full metadata only"
      (("HTTP/1.1 200 OK" `BS.isPrefixOf` out) &&
       expectContentLength 5 out &&
       not (hasHeader "Content-Range" out) &&
       BS.null after)

testFsIfRangeDateExactMatch206 :: Int -> IO Bool
testFsIfRangeDateExactMatch206 port = do
  let p = "/fs/spec_ifrange_date_exact.txt"
  seedFsBody port p "hello"

  mLastMod <- fetchFsHeader port p "Last-Modified"
  case mLastMod of
    Nothing -> assert "If-Range exact-date source Last-Modified available" False
    Just lm ->
      withConn port $ \s -> do
        NSB.sendAll s $
          "GET " <> p <> " HTTP/1.1\r\n" <>
          "Host: localhost\r\n" <>
          "Range: bytes=2-4\r\n" <>
          "If-Range: " <> lm <> "\r\n" <>
          "Connection: close\r\n\r\n"
        mh <- recvHeaders s
        case mh of
          Nothing -> assert "If-Range exact-date 206 headers received" False
          Just (hdrs, rest0) -> do
            let okStatus = statusIs "206 Partial Content" hdrs
                okLen    = expectContentLength 3 hdrs
                okCR     = headerValue "Content-Range" hdrs == Just "bytes 2-4/5"
                need     = 3 - BS.length rest0
            mb <- if need <= 0 then pure (Just (BS.take 3 rest0))
                               else do
                                 mmore <- recvExactly s need
                                 pure ((rest0 <>) <$> mmore)
            case mb of
              Nothing   -> assert "If-Range exact-date 206 reads body" False
              Just body -> assert "If-Range exact date match -> 206"
                (okStatus && okLen && okCR && body == "llo")

testFsIfRangeDateMismatch200 :: Int -> IO Bool
testFsIfRangeDateMismatch200 port = do
  let p = "/fs/spec_ifrange_date_miss.txt"
      oldDate = "Wed, 01 Jan 2020 00:00:00 GMT"
  seedFsBody port p "hello"

  withConn port $ \s -> do
    NSB.sendAll s $
      "GET " <> p <> " HTTP/1.1\r\n" <>
      "Host: localhost\r\n" <>
      "Range: bytes=2-4\r\n" <>
      "If-Range: " <> oldDate <> "\r\n" <>
      "Connection: close\r\n\r\n"
    mh <- recvHeaders s
    case mh of
      Nothing -> assert "If-Range mismatch 200 headers received" False
      Just (hdrs, rest0) -> do
        let okStatus = statusIs "200 OK" hdrs
            okLen    = expectContentLength 5 hdrs
            noCR     = not (hasHeader "Content-Range" hdrs)
            need     = 5 - BS.length rest0
        mb <- if need <= 0 then pure (Just (BS.take 5 rest0))
                           else do
                             mmore <- recvExactly s need
                             pure ((rest0 <>) <$> mmore)
        case mb of
          Nothing   -> assert "If-Range mismatch 200 reads body" False
          Just body -> assert "If-Range date mismatch falls back to full 200"
            (okStatus && okLen && noCR && body == "hello")

testFsIfRangeEtagMatch206 :: Int -> IO Bool
testFsIfRangeEtagMatch206 port = do
  let p = "/fs/spec_ifrange_etag.txt"
  seedFsBody port p "hello"

  mETag <- fetchFsHeader port p "ETag"
  case mETag of
    Nothing -> assert "If-Range ETag source ETag available" False
    Just et ->
      withConn port $ \s -> do
        NSB.sendAll s $
          "GET " <> p <> " HTTP/1.1\r\n" <>
          "Host: localhost\r\n" <>
          "Range: bytes=0-1\r\n" <>
          "If-Range: " <> et <> "\r\n" <>
          "Connection: close\r\n\r\n"
        mh <- recvHeaders s
        case mh of
          Nothing -> assert "If-Range ETag 206 headers received" False
          Just (hdrs, rest0) -> do
            let okStatus = statusIs "206 Partial Content" hdrs
                okLen    = expectContentLength 2 hdrs
                okCR     = headerValue "Content-Range" hdrs == Just "bytes 0-1/5"
                need     = 2 - BS.length rest0
            mb <- if need <= 0 then pure (Just (BS.take 2 rest0))
                               else do
                                 mmore <- recvExactly s need
                                 pure ((rest0 <>) <$> mmore)
            case mb of
              Nothing   -> assert "If-Range ETag 206 reads body" False
              Just body -> assert "If-Range matching ETag -> 206"
                (okStatus && okLen && okCR && body == "he")
