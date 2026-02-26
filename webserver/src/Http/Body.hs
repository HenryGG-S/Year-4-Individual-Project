{-# LANGUAGE OverloadedStrings #-}
module Http.Body
  ( Input(..)
  , BodyError(..)
  , readBodyStrict
  , drainBody
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import System.Timeout (timeout)
import Http.Framing (BodyFraming(..))

data Input = Input
  { inBuf  :: !BS.ByteString
  , inRecv :: Int -> IO BS.ByteString
  }

data BodyError
  = BodyTimedOut
  | BodyClosed
  | BodyTooLarge
  | BodyMalformedChunked
  deriving (Eq, Show)

-- Tuning knobs (keep simple for now)
recvTimeoutMicros :: Int
recvTimeoutMicros = 5000000 -- 5s

maxBodyBytes :: Int
maxBodyBytes = 16 * 1024 * 1024 -- 16MiB cap for strict reads

readBodyStrict :: BodyFraming -> Input -> IO (Either BodyError (BS.ByteString, Input))
readBodyStrict NoBody inp = pure (Right (BS.empty, inp))
readBodyStrict (ContentLength n) inp
  | n < 0 = pure (Left BodyTooLarge)
  | n > maxBodyBytes = pure (Left BodyTooLarge)
  | otherwise = do
      (bs, inp') <- takeExact n inp
      pure (Right (bs, inp'))
readBodyStrict Chunked inp = readChunkedStrict inp

drainBody :: BodyFraming -> Input -> IO (Either BodyError Input)
drainBody NoBody inp = pure (Right inp)
drainBody (ContentLength n) inp
  | n < 0 = pure (Left BodyTooLarge)
  | otherwise = do
      (_bs, inp') <- takeExact n inp
      pure (Right inp')
drainBody Chunked inp = do
  r <- readChunkedStrict inp
  case r of
    Left e        -> pure (Left e)
    Right (_b, i) -> pure (Right i)

-- ===== Internal helpers =====

takeExact :: Int -> Input -> IO (BS.ByteString, Input)
takeExact n inp
  | n <= 0 = pure (BS.empty, inp)
  | BS.length (inBuf inp) >= n =
      let (a, rest) = BS.splitAt n (inBuf inp)
      in pure (a, inp { inBuf = rest })
  | otherwise = go n BS.empty inp
  where
    go k acc i
      | k <= 0 = pure (acc, i)
      | otherwise = do
          i' <- recvMore i
          case i' of
            Nothing -> pure (acc, i { inBuf = BS.empty }) -- closed; caller will treat as malformed/short
            Just j  ->
              if BS.length (inBuf j) >= k
                then
                  let (a, rest) = BS.splitAt k (inBuf j)
                  in pure (acc <> a, j { inBuf = rest })
                else
                  let acc' = acc <> inBuf j
                      k'    = k - BS.length (inBuf j)
                  in go k' acc' (j { inBuf = BS.empty })

recvMore :: Input -> IO (Maybe Input)
recvMore inp = do
  m <- timeout recvTimeoutMicros (inRecv inp 4096)
  case m of
    Nothing -> pure Nothing
    Just bs
      | BS.null bs -> pure Nothing
      | otherwise  -> pure (Just inp { inBuf = inBuf inp <> bs })

-- Read a CRLF-terminated line (excluding CRLF).
readLineCRLF :: Input -> IO (Either BodyError (BS.ByteString, Input))
readLineCRLF inp = go inp
  where
    go i =
      case B8.breakSubstring "\r\n" (inBuf i) of
        (pre, rest)
          | BS.null rest -> do
              mi <- recvMore i
              case mi of
                Nothing -> pure (Left BodyClosed)
                Just j  -> go j
          | otherwise ->
              let remaining = BS.drop 2 rest
              in pure (Right (pre, i { inBuf = remaining }))

readChunkedStrict :: Input -> IO (Either BodyError (BS.ByteString, Input))
readChunkedStrict inp0 = go BS.empty inp0
  where
    go acc inp = do
      eLine <- readLineCRLF inp
      case eLine of
        Left _ -> pure (Left BodyMalformedChunked)
        Right (line0, inp1) -> do
          let line = BS.takeWhile (/= 59) line0 -- strip chunk extensions after ';'
          case parseHex line of
            Nothing -> pure (Left BodyMalformedChunked)
            Just 0  -> do
              -- trailers: read header lines until empty line
              eInp <- drainTrailers inp1
              case eInp of
                Left e    -> pure (Left e)
                Right inpDone -> pure (Right (acc, inpDone))
            Just n
              | n < 0 -> pure (Left BodyMalformedChunked)
              | BS.length acc + n > maxBodyBytes -> pure (Left BodyTooLarge)
              | otherwise -> do
                  (chunk, inp2) <- takeExact n inp1
                  -- after each chunk there must be CRLF
                  eCRLF <- readLineCRLF inp2
                  case eCRLF of
                    Left _ -> pure (Left BodyMalformedChunked)
                    Right (_emptyLine, inp3) ->
                      go (acc <> chunk) inp3

drainTrailers :: Input -> IO (Either BodyError Input)
drainTrailers inp = do
  eLine <- readLineCRLF inp
  case eLine of
    Left _ -> pure (Left BodyMalformedChunked)
    Right (line, inp1) ->
      if BS.null line
        then pure (Right inp1)
        else drainTrailers inp1

parseHex :: BS.ByteString -> Maybe Int
parseHex bs
  | BS.null bs = Nothing
  | otherwise  = BS.foldl' step (Just 0) bs
  where
    step acc w =
      case acc of
        Nothing -> Nothing
        Just n  ->
          let v = hexVal w
          in fmap (\d -> n * 16 + d) v

    hexVal w
      | w >= 48 && w <= 57  = Just (fromIntegral w - 48)  -- 0-9
      | w >= 65 && w <= 70  = Just (fromIntegral w - 55)  -- A-F
      | w >= 97 && w <= 102 = Just (fromIntegral w - 87)  -- a-f
      | otherwise           = Nothing
