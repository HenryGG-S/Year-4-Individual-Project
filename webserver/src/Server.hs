{-# LANGUAGE OverloadedStrings #-}
module Server
  ( runServer
  , startServer
  ) where

import Control.Concurrent (forkFinally)
import Control.Exception (IOException, bracket, catch, try)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.FilePath ((</>), takeDirectory)
import qualified System.Directory as Dir
import Control.Monad (when)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import System.Timeout (timeout)

import Http.Types
import Http.Parse
import Http.Response
import Http.Framing
import Http.Body
import Workloads (BenchPayloads(..), loadBenchPayloads)
import Numeric (showHex)

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

-- ===== Request-head reading =====

data HeadReadResult
  = HeadOk !RequestHead !BS.ByteString
  | HeadClosed
  | HeadBadRequest
  | HeadTooLarge
  | HeadTimedOut

maxHeaderBytes :: Int
maxHeaderBytes = 8192

headRecvTimeoutMicros :: Int
headRecvTimeoutMicros = 5000000

readOneHead :: NS.Socket -> BS.ByteString -> IO HeadReadResult
readOneHead s buf0 = step buf0 (A.parse requestHeadP buf0)
  where
    step _   (A.Done rest req) = pure (HeadOk req rest)
    step _   (A.Fail _ _ _)    = pure HeadBadRequest
    step buf (A.Partial k)
      | BS.length buf > maxHeaderBytes = pure HeadTooLarge
      | otherwise = do
          mChunk <- timeout headRecvTimeoutMicros (NSB.recv s 4096)
          case mChunk of
            Nothing -> pure HeadTimedOut
            Just chunk
              | BS.null chunk ->
                  if BS.null buf
                    then pure HeadClosed
                    else pure HeadBadRequest
              | otherwise ->
                  let buf' = buf <> chunk
                  in if BS.length buf' > maxHeaderBytes
                       then pure HeadTooLarge
                       else step buf' (k chunk)

-- ===== Connection handling =====

handleConn :: BenchPayloads -> NS.Socket -> IO ()
handleConn bench sock = loop BS.empty
  where
    loop buf = do
      e <- readOneHead sock buf
      case e of
        HeadClosed ->
          pure ()

        HeadTimedOut -> do
          resp <- responseFor "HTTP/1.1" requestTimeout ctText "Request Timeout\n" True Close []
          sendBuilder sock resp

        HeadTooLarge -> do
          resp <- responseFor "HTTP/1.1" requestHeaderFieldsTooLarge ctText "Request Header Fields Too Large\n" True Close []
          sendBuilder sock resp

        HeadBadRequest -> do
          resp <- responseFor "HTTP/1.1" badRequest ctText "Bad Request\n" True Close []
          sendBuilder sock resp

        HeadOk headReq rest -> do
          case validateRequestHead headReq of
            Left (st, msg) -> do
              resp <- responseFor (rhVersion headReq) st ctText msg True Close []
              sendBuilder sock resp

            Right () ->
              case validateExpectHeader headReq of
                Left (st, msg) -> do
                  resp <- responseFor (rhVersion headReq) st ctText msg True Close []
                  sendBuilder sock resp

                Right () ->
                  case decideBodyFraming (rhHeaders headReq) of
                    Left _ -> do
                      resp <- responseFor (rhVersion headReq) badRequest ctText "Bad Request\n" True Close []
                      sendBuilder sock resp

                    Right framing ->
                      case validateTargetForOriginServer headReq of
                        Left (st, msg) -> do
                          resp <- responseFor (rhVersion headReq) st ctText msg True Close []
                          sendBuilder sock resp

                        Right path0 -> do
                          let pref  = connectionPref headReq
                              inp0  = Input rest (NSB.recv sock)

                              expect100 =
                                   rhVersion headReq == "HTTP/1.1"
                                && hasExpect100 (rhHeaders headReq)

                              willReadBody =
                                framing /= NoBody &&
                                ( path0 == "/echo"
                                  || case rhMethod headReq of
                                       PUT  -> fsRelPath path0 /= Nothing
                                       POST -> fsRelPath path0 /= Nothing
                                       _    -> False
                                )

                          when (expect100 && willReadBody) $
                            NSB.sendAll sock "HTTP/1.1 100 Continue\r\n\r\n"

                          (inp1, finalPref, resp) <- dispatch bench headReq path0 framing pref inp0
                          sendBuilder sock resp

                          case finalPref of
                            Close     -> pure ()
                            KeepAlive -> loop (inBuf inp1)
-- ===== Dispatch =====

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
    (GET, "/json") ->
      withDrained $ \inp1 ->
        respond inp1 pref ok ctJson (bpJson1k bench) True []

    (HEAD, "/json") ->
      withDrained $ \inp1 ->
        respond inp1 pref ok ctJson (bpJson1k bench) False []

    (GET, "/file50k") ->
      withDrained $ \inp1 ->
        respond inp1 pref ok ctBin (bpFile50k bench) True []

    (HEAD, "/file50k") ->
      withDrained $ \inp1 ->
        respond inp1 pref ok ctBin (bpFile50k bench) False []

    (GET, "/file1m") ->
      withDrained $ \inp1 ->
        respond inp1 pref ok ctBin (bpFile1m bench) True []

    (HEAD, "/file1m") ->
      withDrained $ \inp1 ->
        respond inp1 pref ok ctBin (bpFile1m bench) False []

    -- ===== health/basic =====
    (GET, "/") ->
      withDrained $ \inp1 ->
        respond inp1 pref ok ctText "ok\n" True []

    (HEAD, "/") ->
      withDrained $ \inp1 ->
        respond inp1 pref ok ctText "ok\n" False []

    (GET, "/health") ->
      withDrained $ \inp1 ->
        respond inp1 pref ok ctText "healthy\n" True []

    (HEAD, "/health") ->
      withDrained $ \inp1 ->
        respond inp1 pref ok ctText "healthy\n" False []

    -- ===== echo (for body/framing correctness) =====
    (POST, "/echo") ->
      case framing of
        NoBody ->
          respond inp0 Close lengthRequired ctText "Length Required\n" True []

        _ -> do
          eb <- readBodyStrict framing inp0
          case eb of
            Left e ->
              bodyErrorResult inp0 e
            Right (body, inp1) ->
              respond inp1 pref ok ctBin body True []

    -- ===== filesystem-backed resource routes =====
    (GET, p) | Just rel <- fsRelPath p ->
      withDrained $ \inp1 ->
        serveFsFile ver (rhHeaders headReq) rel True inp1 pref

    (HEAD, p) | Just rel <- fsRelPath p ->
      withDrained $ \inp1 ->
        serveFsFile ver (rhHeaders headReq) rel False inp1 pref

    (PUT, p) | Just rel <- fsRelPath p ->
      putFsFile ver (rhHeaders headReq) rel framing inp0 pref False

    (POST, p) | Just rel <- fsRelPath p ->
      putFsFile ver (rhHeaders headReq) rel framing inp0 pref True

    (DELETE, p) | Just rel <- fsRelPath p ->
      withDrained $ \inp1 ->
        delFsFile ver (rhHeaders headReq) rel inp1 pref

    -- OPTIONS * minimal
    (OPTIONS, "*") ->
      withDrained $ \inp1 ->
        respond inp1 pref ok ctText "ok\n" True
          [("Allow", "GET, HEAD, POST, PUT, DELETE, OPTIONS")]

    -- Unknown / unsupported
    _ ->
      withDrained $ \inp1 ->
        case rhMethod headReq of
          Other _ ->
            respond inp1 Close notImplemented ctText "Not Implemented\n" True []

          _ ->
            case allowedMethodsForPath path of
              Just allow ->
                respond inp1 pref methodNotAllowed ctText "Method Not Allowed\n" True
                  [("Allow", allow)]
              Nothing ->
                respond inp1 pref notFound ctText "Not Found\n" True []

  where
    ver = rhVersion headReq

    respond
      :: Input
      -> ConnectionPref
      -> Status
      -> (BS.ByteString, BS.ByteString)
      -> BS.ByteString
      -> Bool
      -> [(BS.ByteString, BS.ByteString)]
      -> IO (Input, ConnectionPref, BB.Builder)
    respond inp1 pref1 st ct body sendBody extra = do
      resp <- responseFor ver st ct body sendBody pref1 extra
      pure (inp1, pref1, resp)

    withDrained
      :: (Input -> IO (Input, ConnectionPref, BB.Builder))
      -> IO (Input, ConnectionPref, BB.Builder)
    withDrained k = do
      ed <- drainBody framing inp0
      case ed of
        Left e     -> bodyErrorResult inp0 e
        Right inp1 -> k inp1

    bodyErrorResult
      :: Input
      -> BodyError
      -> IO (Input, ConnectionPref, BB.Builder)
    bodyErrorResult inp e = do
      resp <- responseFor ver (bodyErrorStatus e) ctText (bodyErrorMessage e) True Close []
      pure (inp, Close, resp)

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

serveFsFile
  :: BS.ByteString
  -> [Header]
  -> FilePath
  -> Bool
  -> Input
  -> ConnectionPref
  -> IO (Input, ConnectionPref, BB.Builder)
serveFsFile ver reqHeaders rel sendBody inp pref = do
  let full = fsRoot </> rel
  ex <- Dir.doesFileExist full
  if not ex
    then do
      resp <- responseFor ver notFound ctText "not found\n" sendBody pref []
      pure (inp, pref, resp)
    else do
      emtime <- try (Dir.getModificationTime full) :: IO (Either IOException UTCTime)
      esize  <- try (Dir.getFileSize full)         :: IO (Either IOException Integer)
      case (emtime, esize) of
        (Right mtime0, Right size0) -> do
          let mtime = normaliseHttpTime mtime0
              etag  = mkFsEtag mtime size0
              metaHdrs =
                [ ("Last-Modified", formatHttpDate mtime)
                , ("ETag", etag)
                ]

          if matchesIfNoneMatch reqHeaders etag || isNotModified reqHeaders mtime
            then do
              resp <- responseFor ver notModified ctBin "" False pref metaHdrs
              pure (inp, pref, resp)
            else do
              ebs <- try (BS.readFile full) :: IO (Either IOException BS.ByteString)
              case ebs of
                Left _ -> do
                  resp <- responseFor ver internalServerError ctText "IO error\n" True Close []
                  pure (inp, Close, resp)
                Right bs -> do
                  resp <- responseFor ver ok ctBin bs sendBody pref metaHdrs
                  pure (inp, pref, resp)

        _ -> do
          resp <- responseFor ver internalServerError ctText "IO error\n" True Close []
          pure (inp, Close, resp)

putFsFile
  :: BS.ByteString
  -> [Header]
  -> FilePath
  -> BodyFraming
  -> Input
  -> ConnectionPref
  -> Bool
  -> IO (Input, ConnectionPref, BB.Builder)
putFsFile ver reqHeaders rel framing inp pref createOnly = do
  let full = fsRoot </> rel
  ex <- Dir.doesFileExist full

  if createOnly && ex
    then do
      resp <- responseFor ver conflict ctText "Conflict\n" True pref []
      pure (inp, pref, resp)
    else case framing of
      NoBody -> do
        resp <- responseFor ver lengthRequired ctText "Length Required\n" True Close []
        pure (inp, Close, resp)

      _ -> do
        eb <- readBodyStrict framing inp
        case eb of
          Left e -> do
            resp <- responseFor ver (bodyErrorStatus e) ctText (bodyErrorMessage e) True Close []
            pure (inp, Close, resp)

          Right (body, inp1) -> do
            precondOk <-
              if ex
                then do
                  emtime <- try (Dir.getModificationTime full) :: IO (Either IOException UTCTime)
                  esize  <- try (Dir.getFileSize full)         :: IO (Either IOException Integer)
                  case (emtime, esize) of
                    (Right mtime0, Right size0) -> do
                      let mtime = normaliseHttpTime mtime0
                          etag  = mkFsEtag mtime size0
                          metaHdrs =
                            [ ("Last-Modified", formatHttpDate mtime)
                            , ("ETag", etag)
                            ]
                      if passesIfUnmodifiedSince reqHeaders mtime && passesIfMatch reqHeaders etag
                        then pure (Right ())
                        else pure (Left (Just metaHdrs))
                    _ -> pure (Left Nothing)
                else
                  if requiresExistingResource reqHeaders
                    then pure (Left (Just []))
                    else pure (Right ())

            case precondOk of
              Left Nothing -> do
                resp <- responseFor ver internalServerError ctText "IO error\n" True Close []
                pure (inp1, Close, resp)

              Left (Just metaHdrs) -> do
                resp <- responseFor ver preconditionFailed ctText "Precondition Failed\n" True pref metaHdrs
                pure (inp1, pref, resp)

              Right () -> do
                Dir.createDirectoryIfMissing True (takeDirectory full)
                e <- try (BS.writeFile full body) :: IO (Either IOException ())
                case e of
                  Left _ -> do
                    resp <- responseFor ver internalServerError ctText "IO error\n" True Close []
                    pure (inp1, Close, resp)

                  Right _ ->
                    if ex
                      then do
                        resp <- responseFor ver noContent ctText "" False pref []
                        pure (inp1, pref, resp)
                      else do
                        resp <- responseFor ver created ctText "Created\n" True pref []
                        pure (inp1, pref, resp)

delFsFile
  :: BS.ByteString
  -> [Header]
  -> FilePath
  -> Input
  -> ConnectionPref
  -> IO (Input, ConnectionPref, BB.Builder)
delFsFile ver reqHeaders rel inp pref = do
  let full = fsRoot </> rel
  ex <- Dir.doesFileExist full
  if not ex
    then
      if requiresExistingResource reqHeaders
        then do
          resp <- responseFor ver preconditionFailed ctText "Precondition Failed\n" True pref []
          pure (inp, pref, resp)
        else do
          resp <- responseFor ver notFound ctText "not found\n" True pref []
          pure (inp, pref, resp)
    else do
      emtime <- try (Dir.getModificationTime full) :: IO (Either IOException UTCTime)
      esize  <- try (Dir.getFileSize full)         :: IO (Either IOException Integer)
      case (emtime, esize) of
        (Right mtime0, Right size0) -> do
          let mtime = normaliseHttpTime mtime0
              etag  = mkFsEtag mtime size0
              metaHdrs =
                [ ("Last-Modified", formatHttpDate mtime)
                , ("ETag", etag)
                ]

          if not (passesIfUnmodifiedSince reqHeaders mtime && passesIfMatch reqHeaders etag)
            then do
              resp <- responseFor ver preconditionFailed ctText "Precondition Failed\n" True pref metaHdrs
              pure (inp, pref, resp)
            else do
              e <- try (Dir.removeFile full) :: IO (Either IOException ())
              case e of
                Left _ -> do
                  resp <- responseFor ver internalServerError ctText "IO error\n" True Close []
                  pure (inp, Close, resp)
                Right _ -> do
                  resp <- responseFor ver noContent ctText "" False pref []
                  pure (inp, pref, resp)

        _ -> do
          resp <- responseFor ver internalServerError ctText "IO error\n" True Close []
          pure (inp, Close, resp)

stripQuery :: BS.ByteString -> BS.ByteString
stripQuery bs = fst (BS.break (== 63) bs) -- '?'

allowedMethodsForPath :: BS.ByteString -> Maybe BS.ByteString
allowedMethodsForPath path
  | path == "/"        = Just "GET, HEAD"
  | path == "/health"  = Just "GET, HEAD"
  | path == "/json"    = Just "GET, HEAD"
  | path == "/file50k" = Just "GET, HEAD"
  | path == "/file1m"  = Just "GET, HEAD"
  | path == "/echo"    = Just "POST"
  | path == "*"        = Just "OPTIONS"
  | Just _ <- fsRelPath path = Just "GET, HEAD, POST, PUT, DELETE"
  | otherwise = Nothing

bodyErrorStatus :: BodyError -> Status
bodyErrorStatus BodyTooLarge         = payloadTooLarge
bodyErrorStatus BodyTimedOut         = requestTimeout
bodyErrorStatus BodyClosed           = badRequest
bodyErrorStatus BodyMalformedChunked = badRequest

bodyErrorMessage :: BodyError -> BS.ByteString
bodyErrorMessage BodyTooLarge         = "Payload Too Large\n"
bodyErrorMessage BodyTimedOut         = "Request Timeout\n"
bodyErrorMessage BodyClosed           = "Bad Request\n"
bodyErrorMessage BodyMalformedChunked = "Bad Request\n"

validateTargetForOriginServer :: RequestHead -> Either (Status, BS.ByteString) BS.ByteString
validateTargetForOriginServer req =
  case targetView (rhTarget req) of
    TargetInvalid ->
      Left (badRequest, "Bad Request\n")

    TargetAsterisk ->
      case rhMethod req of
        OPTIONS -> Right "*"
        _       -> Left (badRequest, "Bad Request\n")

    TargetAuthority auth ->
      case rhMethod req of
        CONNECT
          | validConnectAuthority auth -> Left (notImplemented, "Not Implemented\n")
          | otherwise                  -> Left (badRequest, "Bad Request\n")
        _ -> Left (badRequest, "Bad Request\n")

    TargetOrigin p ->
      Right (stripQuery p)

    TargetAbsolute _ p ->
      Right (stripQuery p)

validateRequestHead :: RequestHead -> Either (Status, BS.ByteString) ()
validateRequestHead req
  | rhVersion req /= "HTTP/1.1" = Right ()
  | otherwise =
      case headerLookupAll "Host" (rhHeaders req) of
        []  -> Left (badRequest, "Missing Host\n")
        [h] ->
          if validHostValue h
            then Right ()
            else Left (badRequest, "Invalid Host\n")
        _   -> Left (badRequest, "Invalid Host\n")

validateExpectHeader :: RequestHead -> Either (Status, BS.ByteString) ()
validateExpectHeader req
  | rhVersion req /= "HTTP/1.1" = Right ()
  | otherwise =
      case expectTokens (rhHeaders req) of
        [] -> Right ()
        toks
          | all (\t -> CI.mk t == CI.mk "100-continue") toks -> Right ()
          | otherwise -> Left (expectationFailed, "Expectation Failed\n")

expectTokens :: [Header] -> [BS.ByteString]
expectTokens hs =
  [ trimOWS tok
  | raw <- headerLookupAll "Expect" hs
  , tok <- BS.split 44 raw
  , not (BS.null (trimOWS tok))
  ]

trimOWS :: BS.ByteString -> BS.ByteString
trimOWS = dropStart . dropEnd
  where
    isOWS c = c == 32 || c == 9
    dropStart = BS.dropWhile isOWS
    dropEnd b = BS.reverse (BS.dropWhile isOWS (BS.reverse b))

httpDateFormat :: String
httpDateFormat = "%a, %d %b %Y %H:%M:%S GMT"

formatHttpDate :: UTCTime -> BS.ByteString
formatHttpDate =
  B8.pack . formatTime defaultTimeLocale httpDateFormat . normaliseHttpTime

parseHttpDate :: BS.ByteString -> Maybe UTCTime
parseHttpDate raw =
  parseTimeM True defaultTimeLocale httpDateFormat (B8.unpack (trimOWS raw))

normaliseHttpTime :: UTCTime -> UTCTime
normaliseHttpTime =
  posixSecondsToUTCTime . fromInteger . floor . utcTimeToPOSIXSeconds

mkFsEtag :: UTCTime -> Integer -> BS.ByteString
mkFsEtag mtime size0 =
  let secs = floor (utcTimeToPOSIXSeconds (normaliseHttpTime mtime)) :: Integer
      tag  = showHex secs "" <> "-" <> showHex size0 ""
  in B8.pack ("\"" <> tag <> "\"")

matchesIfNoneMatch :: [Header] -> BS.ByteString -> Bool
matchesIfNoneMatch hs etag =
  case headerLookupAll "If-None-Match" hs of
    [] -> False
    vals ->
      any (\raw -> "*" `elemTag` raw || etag `elemTag` raw) vals

passesIfMatch :: [Header] -> BS.ByteString -> Bool
passesIfMatch hs etag =
  case headerLookupAll "If-Match" hs of
    [] -> True
    vals ->
      any (\raw -> "*" `elemTag` raw || etag `elemTag` raw) vals

requiresExistingResource :: [Header] -> Bool
requiresExistingResource hs =
  case headerLookupAll "If-Match" hs of
    [] -> False
    vals -> any (\raw -> "*" `elemTag` raw || not (null (parseEtags raw))) vals

elemTag :: BS.ByteString -> BS.ByteString -> Bool
elemTag needle raw = needle `elem` parseEtags raw

parseEtags :: BS.ByteString -> [BS.ByteString]
parseEtags raw =
  [ normaliseEtTag tok
  | tok0 <- BS.split 44 raw
  , let tok = trimOWS tok0
  , not (BS.null tok)
  ]

normaliseEtTag :: BS.ByteString -> BS.ByteString
normaliseEtTag tok =
  case BS.stripPrefix "W/" tok of
    Just rest -> trimOWS rest
    Nothing   -> tok

isNotModified :: [Header] -> UTCTime -> Bool
isNotModified hs mtime =
  case headerLookupAll "If-Modified-Since" hs of
    [] -> False
    vals ->
      any
        (\raw -> case parseHttpDate raw of
                   Nothing  -> False
                   Just t   -> normaliseHttpTime mtime <= normaliseHttpTime t)
        vals

passesIfUnmodifiedSince :: [Header] -> UTCTime -> Bool
passesIfUnmodifiedSince hs mtime =
  case headerLookupAll "If-Unmodified-Since" hs of
    [] -> True
    vals ->
      any
        (\raw -> case parseHttpDate raw of
                   Nothing -> True
                   Just t  -> normaliseHttpTime mtime <= normaliseHttpTime t)
        vals

-- ===== Response helpers =====

responseFor
  :: BS.ByteString
  -> Status
  -> (BS.ByteString, BS.ByteString)
  -> BS.ByteString
  -> Bool
  -> ConnectionPref
  -> [(BS.ByteString, BS.ByteString)]
  -> IO BB.Builder
responseFor ver st ct body sendBody pref extra = do
  date <- httpDate
  let connHdr = case pref of
        KeepAlive -> ("Connection", "keep-alive")
        Close     -> ("Connection", "close")
      hdrs = [("Date", date), ct] <> extra <> [connHdr]
  pure (mkResponse ver st hdrs body sendBody)

httpDate :: IO BS.ByteString
httpDate =
  B8.pack . formatTime defaultTimeLocale httpDateFormat <$> getCurrentTime

sendBuilder :: NS.Socket -> BB.Builder -> IO ()
sendBuilder c b = go (BB.toLazyByteString b)
  where
    go lbs
      | LBS.null lbs = pure ()
      | otherwise = do
          let (x, xs) = LBS.splitAt 16384 lbs
          NSB.sendAll c (LBS.toStrict x)
          go xs
