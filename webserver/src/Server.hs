{-# LANGUAGE OverloadedStrings #-}
module Server
  ( runServer
  , startServer
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
import System.FilePath ((</>), takeDirectory)
import qualified System.Directory as Dir
import Control.Exception (try)

import Http.Types
import Http.Parse
import Http.Response
import Http.Framing
import Http.Body
import Workloads (BenchPayloads(..), loadBenchPayloads)

-- ===== Public API =====

runServer :: String -> IO ()
runServer port = do
  bench <- loadBenchPayloads "bench_files"
  Dir.createDirectoryIfMissing True fsRoot
  NS.withSocketsDo $ bracket (open port) NS.close (acceptLoop bench)

startServer :: String -> IO (Int, IO ())
startServer port = do
  bench <- loadBenchPayloads "bench_files"
  Dir.createDirectoryIfMissing True fsRoot
  sock <- open port
  chosen <- socketPort sock
  _ <- forkFinally (acceptLoop bench sock) (\_ -> pure ())
  pure (chosen, NS.close sock)

-- ===== Config =====

fsRoot :: FilePath
fsRoot = "fs_root"

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

acceptLoop :: BenchPayloads -> NS.Socket -> IO ()
acceptLoop bench s = go `catch` handler
  where
    handler :: IOException -> IO ()
    handler _ = pure ()
    go = do
      (c, _peer) <- NS.accept s
      _ <- forkFinally (handleConn bench c) (\_ -> NS.close c)
      go

-- ===== Content-Types =====

ctText, ctJson, ctBin :: (BS.ByteString, BS.ByteString)
ctText = ("Content-Type", "text/plain; charset=utf-8")
ctJson = ("Content-Type", "application/json")
ctBin  = ("Content-Type", "application/octet-stream")

-- ===== Connection handling =====

handleConn :: BenchPayloads -> NS.Socket -> IO ()
handleConn bench sock = loop BS.empty
  where
    loop buf = do
      e <- readOneHead sock buf
      case e of
        Left () ->
          sendBuilder sock (responseFor badRequest ctText "Bad Request\n" True Close [])
        Right (Nothing, _) ->
          pure ()
        Right (Just headReq, rest) -> do
          if not (requireHost headReq)
            then sendBuilder sock (responseFor badRequest ctText "Missing Host\n" True Close [])
            else do
              case decideBodyFraming (rhHeaders headReq) of
                Left _ ->
                  sendBuilder sock (responseFor badRequest ctText "Bad Request\n" True Close [])
                Right framing -> do
                  let pref = connectionPref headReq
                      path0 = stripQuery (normalisedPath (rhTarget headReq))
                      inp0  = Input rest (NSB.recv sock)

                  -- Dispatch (may read/drain body)
                  (inp1, finalPref, resp) <- dispatch bench headReq path0 framing pref inp0
                  sendBuilder sock resp

                  case finalPref of
                    Close     -> pure ()
                    KeepAlive -> loop (inBuf inp1)

dispatch
  :: BenchPayloads
  -> RequestHead
  -> BS.ByteString
  -> BodyFraming
  -> ConnectionPref
  -> Input
  -> IO (Input, ConnectionPref, BB.Builder)
dispatch bench headReq path framing pref inp0 =
  case (rhMethod headReq, path) of

    -- ===== benchmark endpoints (preloaded from disk) =====
    (GET, "/json")    -> do inp1 <- drainIf framing inp0
                            pure (inp1, pref, responseFor ok ctJson (bpJson1k bench) True pref [])
    (HEAD, "/json")   -> do inp1 <- drainIf framing inp0
                            pure (inp1, pref, responseFor ok ctJson (bpJson1k bench) False pref [])
    (GET, "/file50k") -> do inp1 <- drainIf framing inp0
                            pure (inp1, pref, responseFor ok ctBin (bpFile50k bench) True pref [])
    (HEAD, "/file50k")-> do inp1 <- drainIf framing inp0
                            pure (inp1, pref, responseFor ok ctBin (bpFile50k bench) False pref [])
    (GET, "/file1m")  -> do inp1 <- drainIf framing inp0
                            pure (inp1, pref, responseFor ok ctBin (bpFile1m bench) True pref [])
    (HEAD, "/file1m") -> do inp1 <- drainIf framing inp0
                            pure (inp1, pref, responseFor ok ctBin (bpFile1m bench) False pref [])

    -- ===== health/basic =====
    (GET, "/")        -> do inp1 <- drainIf framing inp0
                            pure (inp1, pref, responseFor ok ctText "ok\n" True pref [])
    (HEAD, "/")       -> do inp1 <- drainIf framing inp0
                            pure (inp1, pref, responseFor ok ctText "ok\n" False pref [])
    (GET, "/health")  -> do inp1 <- drainIf framing inp0
                            pure (inp1, pref, responseFor ok ctText "healthy\n" True pref [])
    (HEAD, "/health") -> do inp1 <- drainIf framing inp0
                            pure (inp1, pref, responseFor ok ctText "healthy\n" False pref [])

    -- ===== filesystem-backed resource routes =====
    (GET, p) | Just rel <- fsRelPath p -> do
      inp1 <- drainIf framing inp0
      serveFsFile rel True inp1 pref

    (HEAD, p) | Just rel <- fsRelPath p -> do
      inp1 <- drainIf framing inp0
      serveFsFile rel False inp1 pref

    (PUT, p) | Just rel <- fsRelPath p -> do
      putFsFile rel framing inp0 pref False

    (POST, p) | Just rel <- fsRelPath p -> do
      putFsFile rel framing inp0 pref True

    (DELETE, p) | Just rel <- fsRelPath p -> do
      inp1 <- drainIf framing inp0
      delFsFile rel inp1 pref

    -- OPTIONS * minimal
    (OPTIONS, "*") -> do
      inp1 <- drainIf framing inp0
      pure (inp1, pref, responseFor ok ctText "ok\n" True pref
              [("Allow","GET, HEAD, POST, PUT, DELETE, OPTIONS")])

    -- Unknown / unsupported: 501 for unknown token, 405 for known-but-not-allowed
    _ -> do
      inp1 <- drainIf framing inp0
      let resp =
            case rhMethod headReq of
              Other _ -> responseFor notImplemented ctText "Not Implemented\n" True Close []
              _       -> responseFor methodNotAllowed ctText "Method Not Allowed\n" True Close
                          [("Allow","GET, HEAD, POST, PUT, DELETE, OPTIONS")]
      pure (inp1, Close, resp)

-- ===== /fs helpers =====

-- Map "/fs/<path>" to a safe relative FilePath (reject traversal).
fsRelPath :: BS.ByteString -> Maybe FilePath
fsRelPath p =
  case BS.stripPrefix "/fs/" p of
    Nothing -> Nothing
    Just rest ->
      let raw = B8.unpack rest
          segs = split '/' raw
      in if null segs || any badSeg segs then Nothing else Just (foldr1 (</>) segs)
  where
    badSeg s = null s || s == "." || s == ".." || any (== '\0') s

split :: Char -> String -> [String]
split _ "" = []
split c s =
  let (a, b) = break (== c) s
  in a : case b of
           []      -> []
           (_:xs)  -> split c xs

serveFsFile :: FilePath -> Bool -> Input -> ConnectionPref -> IO (Input, ConnectionPref, BB.Builder)
serveFsFile rel sendBody inp pref = do
  let full = fsRoot </> rel
  ex <- Dir.doesFileExist full
  if not ex
    then pure (inp, pref, responseFor notFound ctText "not found\n" sendBody pref [])
    else do
      ebs <- try (BS.readFile full) :: IO (Either IOException BS.ByteString)
      case ebs of
        Left _   -> pure (inp, Close, responseFor internalServerError ctText "IO error\n" True Close [])
        Right bs -> pure (inp, pref, responseFor ok ctBin bs sendBody pref [])

putFsFile :: FilePath -> BodyFraming -> Input -> ConnectionPref -> Bool -> IO (Input, ConnectionPref, BB.Builder)
putFsFile rel framing inp pref createOnly = do
  let full = fsRoot </> rel
  ex <- Dir.doesFileExist full
  if createOnly && ex
    then pure (inp, pref, responseFor conflict ctText "Conflict\n" True pref [])
    else case framing of
      NoBody -> pure (inp, Close, responseFor lengthRequired ctText "Length Required\n" True Close [])
      _ -> do
        eb <- readBodyStrict framing inp
        case eb of
          Left _ -> pure (inp, Close, responseFor payloadTooLarge ctText "Payload Too Large\n" True Close [])
          Right (body, inp1) -> do
            Dir.createDirectoryIfMissing True (takeDirectory full)
            e <- try (BS.writeFile full body) :: IO (Either IOException ())
            case e of
              Left _ ->
                pure (inp1, Close, responseFor internalServerError ctText "IO error\n" True Close [])
              Right _ ->
                if ex
                  then pure (inp1, pref, responseFor noContent ctText "" True pref [])
                  else pure (inp1, pref, responseFor created ctText "Created\n" True pref [])

delFsFile :: FilePath -> Input -> ConnectionPref -> IO (Input, ConnectionPref, BB.Builder)
delFsFile rel inp pref = do
  let full = fsRoot </> rel
  ex <- Dir.doesFileExist full
  if not ex
    then pure (inp, pref, responseFor notFound ctText "not found\n" True pref [])
    else do
      e <- try (Dir.removeFile full) :: IO (Either IOException ())
      case e of
        Left _  -> pure (inp, Close, responseFor internalServerError ctText "IO error\n" True Close [])
        Right _ -> pure (inp, pref, responseFor noContent ctText "" True pref [])

stripQuery :: BS.ByteString -> BS.ByteString
stripQuery bs = fst (BS.break (== 63) bs) -- '?'

drainIf :: BodyFraming -> Input -> IO Input
drainIf framing inp = do
  r <- drainBody framing inp
  case r of
    Left _  -> pure inp
    Right i -> pure i

-- ===== Reading the request head (incremental) =====

maxHeaderBytes :: Int
maxHeaderBytes = 8192

readOneHead :: NS.Socket -> BS.ByteString -> IO (Either () (Maybe RequestHead, BS.ByteString))
readOneHead s buf0 = step buf0 (A.parse requestHeadP buf0)
  where
    step _ (A.Done rest req) = pure (Right (Just req, rest))
    step _   (A.Fail _ _ _)    = pure (Left ())
    step buf (A.Partial k) =
      if BS.length buf > maxHeaderBytes
        then pure (Left ())
        else do
          chunk <- NSB.recv s 4096
          if BS.null chunk
            then pure (Right (Nothing, BS.empty))
            else
              let buf' = buf <> chunk
              in step buf' (k chunk)

-- ===== Response helpers =====

responseFor
  :: Status
  -> (BS.ByteString, BS.ByteString)
  -> BS.ByteString
  -> Bool
  -> ConnectionPref
  -> [(BS.ByteString, BS.ByteString)]
  -> BB.Builder
responseFor st ct body sendBody pref extra =
  let connHdr = case pref of
        KeepAlive -> ("Connection","keep-alive")
        Close     -> ("Connection","close")
      hdrs = [ct] <> extra <> [connHdr]
  in mkResponse st hdrs body sendBody

sendBuilder :: NS.Socket -> BB.Builder -> IO ()
sendBuilder c b = go (BB.toLazyByteString b)
  where
    go lbs
      | LBS.null lbs = pure ()
      | otherwise = do
          let (x, xs) = LBS.splitAt 16384 lbs
          NSB.sendAll c (LBS.toStrict x)
          go xs
