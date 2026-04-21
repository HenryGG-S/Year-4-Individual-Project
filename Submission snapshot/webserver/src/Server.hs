{-# LANGUAGE OverloadedStrings #-}
module Server
  ( runServer
  , startServer
  ) where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (IOException, bracket, catch, try, evaluate)
import Control.Monad (when, unless)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Char (toLower)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Maybe (mapMaybe)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import Numeric (showHex)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory, takeExtension)
import qualified System.Directory as Dir
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Control.Applicative ((<|>))
import Text.Read (readMaybe)

import Http.Body
import Http.Framing
import Http.Parse
import Http.Response
import Http.Types
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

ctForFsPath :: FilePath -> (BS.ByteString, BS.ByteString)
ctForFsPath rel =
  case map toLower (takeExtension rel) of
    ".txt"  -> ("Content-Type", "text/plain; charset=utf-8")
    ".html" -> ("Content-Type", "text/html; charset=utf-8")
    ".htm"  -> ("Content-Type", "text/html; charset=utf-8")
    ".css"  -> ("Content-Type", "text/css; charset=utf-8")
    ".js"   -> ("Content-Type", "application/javascript; charset=utf-8")
    ".json" -> ("Content-Type", "application/json")
    ".csv"  -> ("Content-Type", "text/csv; charset=utf-8")
    ".xml"  -> ("Content-Type", "application/xml")
    ".png"  -> ("Content-Type", "image/png")
    ".jpg"  -> ("Content-Type", "image/jpeg")
    ".jpeg" -> ("Content-Type", "image/jpeg")
    ".gif"  -> ("Content-Type", "image/gif")
    ".svg"  -> ("Content-Type", "image/svg+xml")
    ".pdf"  -> ("Content-Type", "application/pdf")
    _       -> ctBin

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
                    Left ferr -> do
                      let (st, msg) = framingErrorResult ferr
                      resp <- responseFor (rhVersion headReq) st ctText msg True Close []
                      sendBuilder sock resp

                    Right framing ->
                      case validateTargetForOriginServer headReq of
                        Left (st, msg) -> do
                          resp <- responseFor (rhVersion headReq) st ctText msg True Close []
                          sendBuilder sock resp

                        Right path0 -> do
                          let pref = connectionPref headReq
                              inp0 = Input rest (NSB.recv sock)

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


                          mTraceSeed <-
                            if rhMethod headReq == GET
                              then maybeStartTrace path0
                              else pure Nothing

                          (inp1, finalPref, resp, mPendingTrace) <-
                            dispatch bench headReq path0 framing pref inp0 mTraceSeed
                          sendBuilderMaybeProfiled sock resp mPendingTrace

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
  -> Maybe TraceSeed
  -> IO (Input, ConnectionPref, BB.Builder, Maybe PendingTrace)
dispatch bench headReq path framing pref inp0 mTraceSeed =
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
        noTrace $
          serveFsFile ver GET (rhHeaders headReq) rel True inp1 pref

    (HEAD, p) | Just rel <- fsRelPath p ->
      withDrained $ \inp1 ->
        noTrace $
          serveFsFile ver HEAD (rhHeaders headReq) rel False inp1 pref

    (PUT, p) | Just rel <- fsRelPath p ->
      noTrace $
        putFsFile ver PUT (rhHeaders headReq) rel framing inp0 pref False

    (POST, p) | Just rel <- fsRelPath p ->
      noTrace $
        putFsFile ver POST (rhHeaders headReq) rel framing inp0 pref True

    (DELETE, p) | Just rel <- fsRelPath p ->
      withDrained $ \inp1 ->
        noTrace $
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

    noTrace
      :: IO (Input, ConnectionPref, BB.Builder)
      -> IO (Input, ConnectionPref, BB.Builder, Maybe PendingTrace)
    noTrace action = do
      (inp1, pref1, resp) <- action
      pure (inp1, pref1, resp, Nothing)

    respond
      :: Input
      -> ConnectionPref
      -> Status
      -> (BS.ByteString, BS.ByteString)
      -> BS.ByteString
      -> Bool
      -> [(BS.ByteString, BS.ByteString)]
      -> IO (Input, ConnectionPref, BB.Builder, Maybe PendingTrace)
    respond inp1 pref1 st ct body sendBody extra =
      case mTraceSeed of
        Nothing -> do
          resp <- responseFor ver st ct body sendBody pref1 extra
          pure (inp1, pref1, resp, Nothing)

        Just seed -> do
          (resp, buildNs) <- timedNs $
            responseFor ver st ct body sendBody pref1 extra
          pure (inp1, pref1, resp, Just (finishTraceBuild seed buildNs))

    withDrained
      :: (Input -> IO (Input, ConnectionPref, BB.Builder, Maybe PendingTrace))
      -> IO (Input, ConnectionPref, BB.Builder, Maybe PendingTrace)
    withDrained k = do
      ed <- drainBody framing inp0
      case ed of
        Left e     -> bodyErrorResult inp0 e
        Right inp1 -> k inp1

    bodyErrorResult
      :: Input
      -> BodyError
      -> IO (Input, ConnectionPref, BB.Builder, Maybe PendingTrace)
    bodyErrorResult inp e = do
      resp <- responseFor ver (bodyErrorStatus e) ctText (bodyErrorMessage e) True Close []
      pure (inp, Close, resp, Nothing)

-- ===== /fs helpers =====

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
           []     -> []
           (_:xs) -> split c xs

data ResourceState = ResourceState
  { rsMtime :: !UTCTime
  , rsSize  :: !Integer
  , rsEtag  :: !EntityTag
  }

data EntityTag = EntityTag
  { etWeak   :: !Bool
  , etOpaque :: !BS.ByteString
  } deriving (Eq, Show)

data PreconditionOutcome
  = PreconditionsPass
  | PreconditionsNotModified
  | PreconditionsFailed
  deriving (Eq, Show)

data RangeDecision
  = ServeFull
  | ServePartial !Integer !Integer
  | ServeRangeUnsat
  deriving (Eq, Show)

serveFsFile
  :: BS.ByteString
  -> Method
  -> [Header]
  -> FilePath
  -> Bool
  -> Input
  -> ConnectionPref
  -> IO (Input, ConnectionPref, BB.Builder)
serveFsFile ver meth reqHeaders rel sendBody inp pref = do
  let full = fsRoot </> rel
      ct   = ctForFsPath rel
  ex <- Dir.doesFileExist full
  if not ex
    then do
      resp <- responseFor ver notFound ctText "not found\n" sendBody pref []
      pure (inp, pref, resp)
    else do
      estate <- loadResourceState full
      case estate of
        Left _ -> do
          resp <- responseFor ver internalServerError ctText "IO error\n" True Close []
          pure (inp, Close, resp)

        Right st -> do
          let metaHdrs = resourceMetaHeaders st
          case evaluatePreconditions meth reqHeaders (Just st) of
            PreconditionsFailed -> do
              resp <- responseFor ver preconditionFailed ctText "Precondition Failed\n" True pref metaHdrs
              pure (inp, pref, resp)

            PreconditionsNotModified -> do
              resp <- responseFor ver notModified ct "" False pref metaHdrs
              pure (inp, pref, resp)

            PreconditionsPass -> do
              ebs <- try (BS.readFile full) :: IO (Either IOException BS.ByteString)
              case ebs of
                Left _ -> do
                  resp <- responseFor ver internalServerError ctText "IO error\n" True Close []
                  pure (inp, Close, resp)

                Right bs ->
                  let rangeDecision =
                        case meth of
                          GET -> decideRange reqHeaders st
                          _   -> ServeFull
                  in case rangeDecision of
                       ServeFull -> do
                         resp <- responseFor ver ok ct bs sendBody pref metaHdrs
                         pure (inp, pref, resp)

                       ServeRangeUnsat -> do
                         let extra =
                               metaHdrs
                                 ++ [("Content-Range", unsatisfiedContentRange (rsSize st))]
                         resp <- responseFor ver rangeNotSatisfiable ctText "Range Not Satisfiable\n" True pref extra
                         pure (inp, pref, resp)

                       ServePartial start end -> do
                         let bodyPart = sliceBytes start end bs
                             extra =
                               metaHdrs
                                 ++ [("Content-Range", satisfiedContentRange start end (rsSize st))]
                         resp <- responseFor ver partialContent ct bodyPart sendBody pref extra
                         pure (inp, pref, resp)

putFsFile
  :: BS.ByteString
  -> Method
  -> [Header]
  -> FilePath
  -> BodyFraming
  -> Input
  -> ConnectionPref
  -> Bool
  -> IO (Input, ConnectionPref, BB.Builder)
putFsFile ver meth reqHeaders rel framing inp pref createOnly = do
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
            mState <-
              if ex
                then do
                  estate <- loadResourceState full
                  pure (either (const Nothing) Just estate)
                else pure Nothing

            let metaHdrs = maybe [] resourceMetaHeaders mState

            case evaluatePreconditions meth reqHeaders mState of
              PreconditionsFailed -> do
                resp <- responseFor ver preconditionFailed ctText "Precondition Failed\n" True pref metaHdrs
                pure (inp1, pref, resp)

              PreconditionsNotModified -> do
                resp <- responseFor ver preconditionFailed ctText "Precondition Failed\n" True pref metaHdrs
                pure (inp1, pref, resp)

              PreconditionsPass -> do
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
      case evaluatePreconditions DELETE reqHeaders Nothing of
        PreconditionsFailed -> do
          resp <- responseFor ver preconditionFailed ctText "Precondition Failed\n" True pref []
          pure (inp, pref, resp)
        _ -> do
          resp <- responseFor ver notFound ctText "not found\n" True pref []
          pure (inp, pref, resp)
    else do
      estate <- loadResourceState full
      case estate of
        Left _ -> do
          resp <- responseFor ver internalServerError ctText "IO error\n" True Close []
          pure (inp, Close, resp)

        Right st -> do
          let metaHdrs = resourceMetaHeaders st
          case evaluatePreconditions DELETE reqHeaders (Just st) of
            PreconditionsFailed -> do
              resp <- responseFor ver preconditionFailed ctText "Precondition Failed\n" True pref metaHdrs
              pure (inp, pref, resp)

            PreconditionsNotModified -> do
              resp <- responseFor ver preconditionFailed ctText "Precondition Failed\n" True pref metaHdrs
              pure (inp, pref, resp)

            PreconditionsPass -> do
              e <- try (Dir.removeFile full) :: IO (Either IOException ())
              case e of
                Left _ -> do
                  resp <- responseFor ver internalServerError ctText "IO error\n" True Close []
                  pure (inp, Close, resp)
                Right _ -> do
                  resp <- responseFor ver noContent ctText "" False pref []
                  pure (inp, pref, resp)

loadResourceState :: FilePath -> IO (Either IOException ResourceState)
loadResourceState full = do
  emtime <- try (Dir.getModificationTime full) :: IO (Either IOException UTCTime)
  esize  <- try (Dir.getFileSize full)         :: IO (Either IOException Integer)
  pure $ do
    mtime0 <- emtime
    size0  <- esize
    let mtime = normaliseHttpTime mtime0
    pure ResourceState
      { rsMtime = mtime
      , rsSize  = size0
      , rsEtag  = mkFsEtag mtime size0
      }

resourceMetaHeaders :: ResourceState -> [(BS.ByteString, BS.ByteString)]
resourceMetaHeaders st =
  [ ("Last-Modified", formatHttpDate (rsMtime st))
  , ("ETag", renderEntityTag (rsEtag st))
  , ("Accept-Ranges", "bytes")
  ]

stripQuery :: BS.ByteString -> BS.ByteString
stripQuery bs = fst (BS.break (== 63) bs)

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
      parseFmt rfc1123
  <|> parseFmt rfc850
  <|> parseFmt asctimeFmt
  where
    s = B8.unpack (trimOWS raw)
    parseFmt fmt = parseTimeM True defaultTimeLocale fmt s
    rfc1123   = "%a, %d %b %Y %H:%M:%S GMT"
    rfc850    = "%A, %d-%b-%y %H:%M:%S GMT"
    asctimeFmt = "%a %b %e %H:%M:%S %Y"

normaliseHttpTime :: UTCTime -> UTCTime
normaliseHttpTime =
  posixSecondsToUTCTime . fromInteger . floor . utcTimeToPOSIXSeconds

mkFsEtag :: UTCTime -> Integer -> EntityTag
mkFsEtag mtime size0 =
  let secs = floor (utcTimeToPOSIXSeconds (normaliseHttpTime mtime)) :: Integer
      tag  = B8.pack (showHex secs "" <> "-" <> showHex size0 "")
  in EntityTag False tag

renderEntityTag :: EntityTag -> BS.ByteString
renderEntityTag (EntityTag weak opaque) =
  (if weak then "W/" else "") <> "\"" <> opaque <> "\""

parseEntityTag :: BS.ByteString -> Maybe EntityTag
parseEntityTag raw0 =
  let raw = trimOWS raw0
  in case BS.stripPrefix "W/" raw of
       Just rest -> EntityTag True <$> parseQuotedOpaque rest
       Nothing   -> EntityTag False <$> parseQuotedOpaque raw

parseQuotedOpaque :: BS.ByteString -> Maybe BS.ByteString
parseQuotedOpaque bs = do
  inner0 <- BS.stripPrefix "\"" bs
  case BS.unsnoc inner0 of
    Just (inner, 34) -> Just inner
    _                -> Nothing

data ETagToken = AnyTag | SpecificTag EntityTag

parseETagList :: BS.ByteString -> [ETagToken]
parseETagList raw =
  mapMaybe parseOne (BS.split 44 raw)
  where
    parseOne tok0 =
      let tok = trimOWS tok0
      in if tok == "*"
           then Just AnyTag
           else SpecificTag <$> parseEntityTag tok

strongCompare :: EntityTag -> EntityTag -> Bool
strongCompare a b =
  not (etWeak a) && not (etWeak b) && etOpaque a == etOpaque b

weakCompare :: EntityTag -> EntityTag -> Bool
weakCompare a b =
  etOpaque a == etOpaque b

hasIfMatchHeader :: [Header] -> Bool
hasIfMatchHeader hs = not (null (headerLookupAll "If-Match" hs))

hasIfNoneMatchHeader :: [Header] -> Bool
hasIfNoneMatchHeader hs = not (null (headerLookupAll "If-None-Match" hs))

ifMatchSatisfied :: [Header] -> Maybe ResourceState -> Bool
ifMatchSatisfied hs mState =
  case concatMap parseETagList (headerLookupAll "If-Match" hs) of
    [] -> True
    toks ->
      case mState of
        Nothing -> False
        Just st ->
          any (matchesCurrentStrong (rsEtag st)) toks
  where
    matchesCurrentStrong _ AnyTag             = True
    matchesCurrentStrong cur (SpecificTag et) = strongCompare cur et

ifNoneMatchMatched :: [Header] -> Maybe ResourceState -> Bool
ifNoneMatchMatched hs mState =
  case concatMap parseETagList (headerLookupAll "If-None-Match" hs) of
    [] -> False
    toks ->
      case mState of
        Nothing -> False
        Just st ->
          any (matchesCurrentWeak (rsEtag st)) toks
  where
    matchesCurrentWeak _   AnyTag             = True
    matchesCurrentWeak cur (SpecificTag et)   = weakCompare cur et

isNotModifiedSince :: [Header] -> Maybe ResourceState -> Bool
isNotModifiedSince hs mState =
  case mState of
    Nothing -> False
    Just st ->
      any
        (\raw -> case parseHttpDate raw of
                   Nothing -> False
                   Just t  -> rsMtime st <= normaliseHttpTime t)
        (headerLookupAll "If-Modified-Since" hs)

passesIfUnmodifiedSince :: [Header] -> Maybe ResourceState -> Bool
passesIfUnmodifiedSince hs mState =
  case headerLookupAll "If-Unmodified-Since" hs of
    [] -> True
    vals ->
      case mState of
        Nothing -> True
        Just st ->
          any
            (\raw -> case parseHttpDate raw of
                       Nothing -> True
                       Just t  -> rsMtime st <= normaliseHttpTime t)
            vals

methodIsRetrieval :: Method -> Bool
methodIsRetrieval GET  = True
methodIsRetrieval HEAD = True
methodIsRetrieval _    = False

evaluatePreconditions :: Method -> [Header] -> Maybe ResourceState -> PreconditionOutcome
evaluatePreconditions meth hs mState
  | hasIfMatchHeader hs && not (ifMatchSatisfied hs mState) =
      PreconditionsFailed
  | not (hasIfMatchHeader hs) && not (passesIfUnmodifiedSince hs mState) =
      PreconditionsFailed
  | hasIfNoneMatchHeader hs && ifNoneMatchMatched hs mState =
      if methodIsRetrieval meth
        then PreconditionsNotModified
        else PreconditionsFailed
  | not (hasIfNoneMatchHeader hs)
      && methodIsRetrieval meth
      && isNotModifiedSince hs mState =
      PreconditionsNotModified
  | otherwise =
      PreconditionsPass

data IfRangeValue
  = IfRangeETag !EntityTag
  | IfRangeDate !UTCTime

parseIfRangeValue :: BS.ByteString -> Maybe IfRangeValue
parseIfRangeValue raw =
  case parseEntityTag (trimOWS raw) of
    Just et -> Just (IfRangeETag et)
    Nothing -> IfRangeDate <$> parseHttpDate raw

ifRangeAllows :: [Header] -> ResourceState -> Bool
ifRangeAllows hs st =
  case headerLookupAll "If-Range" hs of
    [] -> True
    raw:_ ->
      case parseIfRangeValue raw of
        Just (IfRangeETag et) -> strongCompare (rsEtag st) et
        Just (IfRangeDate t)  -> rsMtime st == normaliseHttpTime t
        Nothing               -> False

decideRange :: [Header] -> ResourceState -> RangeDecision
decideRange hs st =
  case headerLookupAll "Range" hs of
    [] -> ServeFull
    raw:_ ->
      if not (ifRangeAllows hs st)
        then ServeFull
        else parseSingleRange raw (rsSize st)

parseSingleRange :: BS.ByteString -> Integer -> RangeDecision
parseSingleRange raw0 totalSize =
  let raw = trimOWS raw0
  in case BS.stripPrefix "bytes=" raw of
       Nothing   -> ServeFull
       Just spec ->
         if BS.elem 44 spec
           then ServeFull
           else
             let (lhs0, rhsWithDash) = BS.break (== 45) spec
             in case BS.uncons rhsWithDash of
                  Nothing -> ServeFull
                  Just (45, rhs0) ->
                    let lhs = trimOWS lhs0
                        rhs = trimOWS rhs0
                    in case (parseDecMaybe lhs, parseDecMaybe rhs) of
                         (Nothing, Nothing) -> ServeFull
                         (Just _, Nothing) ->
                           case parseDecMaybe lhs of
                             Just start
                               | totalSize <= 0      -> ServeRangeUnsat
                               | start >= totalSize  -> ServeRangeUnsat
                               | otherwise           -> ServePartial start (totalSize - 1)
                             _ -> ServeFull
                         (Nothing, Just suffixLen)
                           | suffixLen <= 0 -> ServeFull
                           | totalSize <= 0 -> ServeRangeUnsat
                           | suffixLen >= totalSize -> ServePartial 0 (totalSize - 1)
                           | otherwise ->
                               ServePartial (totalSize - suffixLen) (totalSize - 1)
                         (Just start, Just end)
                           | start > end    -> ServeFull
                           | totalSize <= 0 -> ServeRangeUnsat
                           | start >= totalSize -> ServeRangeUnsat
                           | otherwise      -> ServePartial start (min end (totalSize - 1))

parseDecMaybe :: BS.ByteString -> Maybe Integer
parseDecMaybe bs
  | BS.null bs = Nothing
  | BS.any (\w -> w < 48 || w > 57) bs = Nothing
  | otherwise = Just (BS.foldl' (\acc w -> acc * 10 + toInteger (w - 48)) 0 bs)

sliceBytes :: Integer -> Integer -> BS.ByteString -> BS.ByteString
sliceBytes start end =
  BS.take (fromIntegral (end - start + 1)) . BS.drop (fromIntegral start)

satisfiedContentRange :: Integer -> Integer -> Integer -> BS.ByteString
satisfiedContentRange start end total =
  "bytes " <> bsShowI start <> "-" <> bsShowI end <> "/" <> bsShowI total

unsatisfiedContentRange :: Integer -> BS.ByteString
unsatisfiedContentRange total =
  "bytes */" <> bsShowI total

bsShowI :: Integer -> BS.ByteString
bsShowI = B8.pack . show

-- ===== Lightweight stage profiling =====

data TraceCfg = TraceCfg
  { tcEnabled     :: !Bool
  , tcSampleEvery :: !Int
  , tcOutCsv      :: !FilePath
  }

data TraceSeed = TraceSeed
  { tsLabel      :: !BS.ByteString
  , tsReqStartNs :: !Word64
  }

data PendingTrace = PendingTrace
  { ptLabel      :: !BS.ByteString
  , ptReqStartNs :: !Word64
  , ptBuildNs    :: !Word64
  }

{-# NOINLINE traceCfg #-}
traceCfg :: TraceCfg
traceCfg = unsafePerformIO initTraceCfg

{-# NOINLINE traceCounter #-}
traceCounter :: IORef Int
traceCounter = unsafePerformIO (newIORef 0)

{-# NOINLINE traceCsvLock #-}
traceCsvLock :: MVar ()
traceCsvLock = unsafePerformIO (newMVar ())

initTraceCfg :: IO TraceCfg
initTraceCfg = do
  mEnabled <- lookupEnv "PROFILE_STAGES"
  mEvery   <- lookupEnv "PROFILE_SAMPLE_EVERY"
  mOut     <- lookupEnv "PROFILE_STAGE_CSV"
  let enabled =
        case fmap (map toLower) mEnabled of
          Just "1"    -> True
          Just "true" -> True
          Just "yes"  -> True
          Just "on"   -> True
          _           -> False

      sampleEvery =
        max 1 $
          case mEvery >>= readMaybe of
            Just n  -> n
            Nothing -> 50

      outCsv =
        case mOut of
          Just p  -> p
          Nothing -> "bench/stage_times.csv"

  pure TraceCfg
    { tcEnabled = enabled
    , tcSampleEvery = sampleEvery
    , tcOutCsv = outCsv
    }

shouldTracePath :: BS.ByteString -> Bool
shouldTracePath p =
  p == "/json" || p == "/file1m"

timedNs :: IO a -> IO (a, Word64)
timedNs action = do
  t0 <- getMonotonicTimeNSec
  x  <- action
  t1 <- getMonotonicTimeNSec
  pure (x, t1 - t0)

maybeStartTrace :: BS.ByteString -> IO (Maybe TraceSeed)
maybeStartTrace path
  | not (tcEnabled traceCfg) = pure Nothing
  | not (shouldTracePath path) = pure Nothing
  | otherwise = do
      n <- atomicModifyIORef' traceCounter $ \i ->
        let j = i + 1
        in (j, j)

      if n `mod` tcSampleEvery traceCfg == 0
        then do
          t0 <- getMonotonicTimeNSec
          pure (Just (TraceSeed path t0))
        else pure Nothing

finishTraceBuild :: TraceSeed -> Word64 -> PendingTrace
finishTraceBuild seed buildNs =
  PendingTrace
    { ptLabel = tsLabel seed
    , ptReqStartNs = tsReqStartNs seed
    , ptBuildNs = buildNs
    }

appendTraceCsv :: PendingTrace -> Word64 -> Word64 -> Word64 -> IO ()
appendTraceCsv pending toLazyNs sendNs totalNs =
  withMVar traceCsvLock $ \_ -> do
    let out = tcOutCsv traceCfg
    Dir.createDirectoryIfMissing True (takeDirectory out)
    exists <- Dir.doesFileExist out
    unless exists $
      appendFile out "endpoint,build_ns,to_lazy_ns,send_ns,total_ns\n"

    appendFile out $
         B8.unpack (ptLabel pending) <> ","
      <> show (ptBuildNs pending)    <> ","
      <> show toLazyNs               <> ","
      <> show sendNs                 <> ","
      <> show totalNs                <> "\n"

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
sendBuilder c b =
  sendLazyChunks c (BB.toLazyByteString b)

sendBuilderMaybeProfiled :: NS.Socket -> BB.Builder -> Maybe PendingTrace -> IO ()
sendBuilderMaybeProfiled c b Nothing =
  sendBuilder c b

sendBuilderMaybeProfiled c b (Just pending) = do
  (lbs, toLazyNs) <- timedNs $ do
    let lbs = BB.toLazyByteString b
    _ <- evaluate lbs
    pure lbs

  (_, sendNs) <- timedNs $
    sendLazyChunks c lbs

  t1 <- getMonotonicTimeNSec
  let totalNs = t1 - ptReqStartNs pending

  appendTraceCsv pending toLazyNs sendNs totalNs

sendLazyChunks :: NS.Socket -> LBS.ByteString -> IO ()
sendLazyChunks c lbs
  | LBS.null lbs = pure ()
  | otherwise = do
      let (x, xs) = LBS.splitAt 16384 lbs
      NSB.sendAll c (LBS.toStrict x)
      sendLazyChunks c xs

framingErrorResult :: FramingError -> (Status, BS.ByteString)
framingErrorResult ferr =
  case ferr of
    ConflictingLength           -> (badRequest, "Bad Request\n")
    InvalidContentLength        -> (badRequest, "Bad Request\n")
    UnsupportedTransferEncoding -> (notImplemented, "Not Implemented\n")
