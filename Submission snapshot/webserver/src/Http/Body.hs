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
import Data.Word (Word8)

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

recvTimeoutMicros :: Int
recvTimeoutMicros = 5000000

maxBodyBytes :: Int
maxBodyBytes = 16 * 1024 * 1024

readBodyStrict :: BodyFraming -> Input -> IO (Either BodyError (BS.ByteString, Input))
readBodyStrict NoBody inp = pure (Right (BS.empty, inp))
readBodyStrict (ContentLength n) inp
  | n < 0 = pure (Left BodyTooLarge)
  | n > maxBodyBytes = pure (Left BodyTooLarge)
  | otherwise = takeExact n inp
readBodyStrict Chunked inp = readChunkedStrict inp

drainBody :: BodyFraming -> Input -> IO (Either BodyError Input)
drainBody NoBody inp = pure (Right inp)
drainBody (ContentLength n) inp
  | n < 0 = pure (Left BodyTooLarge)
  | otherwise = drainExact n inp
drainBody Chunked inp = drainChunked inp

recvMore :: Input -> IO (Either BodyError Input)
recvMore inp = do
  m <- timeout recvTimeoutMicros (inRecv inp 4096)
  case m of
    Nothing -> pure (Left BodyTimedOut)
    Just bs
      | BS.null bs -> pure (Left BodyClosed)
      | otherwise  -> pure (Right inp { inBuf = inBuf inp <> bs })

takeExact :: Int -> Input -> IO (Either BodyError (BS.ByteString, Input))
takeExact n inp
  | n <= 0 = pure (Right (BS.empty, inp))
  | BS.length (inBuf inp) >= n =
      let (a, rest) = BS.splitAt n (inBuf inp)
      in pure (Right (a, inp { inBuf = rest }))
  | otherwise =
      let acc0 = inBuf inp
          k0   = n - BS.length acc0
      in go k0 acc0 (inp { inBuf = BS.empty })
  where
    go k acc i
      | k <= 0 = pure (Right (acc, i))
      | otherwise = do
          ei' <- recvMore i
          case ei' of
            Left e -> pure (Left e)
            Right j ->
              if BS.length (inBuf j) >= k
                then
                  let (a, rest) = BS.splitAt k (inBuf j)
                  in pure (Right (acc <> a, j { inBuf = rest }))
                else
                  let chunk = inBuf j
                      acc'  = acc <> chunk
                      k'    = k - BS.length chunk
                  in go k' acc' (j { inBuf = BS.empty })

drainExact :: Int -> Input -> IO (Either BodyError Input)
drainExact n inp
  | n <= 0 = pure (Right inp)
  | BS.length (inBuf inp) >= n =
      pure (Right inp { inBuf = BS.drop n (inBuf inp) })
  | otherwise =
      let remaining = n - BS.length (inBuf inp)
      in go remaining (inp { inBuf = BS.empty })
  where
    go k i
      | k <= 0 = pure (Right i)
      | otherwise = do
          ei' <- recvMore i
          case ei' of
            Left e -> pure (Left e)
            Right j ->
              let got = BS.length (inBuf j)
              in if got >= k
                   then pure (Right j { inBuf = BS.drop k (inBuf j) })
                   else go (k - got) (j { inBuf = BS.empty })

readLineCRLF :: Input -> IO (Either BodyError (BS.ByteString, Input))
readLineCRLF inp = go inp
  where
    go i
      | BS.length (inBuf i) > maxChunkLineBytes = pure (Left BodyMalformedChunked)
      | otherwise =
          case B8.breakSubstring "\r\n" (inBuf i) of
            (pre, rest)
              | BS.null rest -> do
                  ei' <- recvMore i
                  case ei' of
                    Left e  -> pure (Left e)
                    Right j -> go j
              | otherwise ->
                  let remaining = BS.drop 2 rest
                  in pure (Right (pre, i { inBuf = remaining }))

readChunkedStrict :: Input -> IO (Either BodyError (BS.ByteString, Input))
readChunkedStrict inp0 = go BS.empty inp0
  where
    go acc inp = do
      eLine <- readLineCRLF inp
      case eLine of
        Left e -> pure (Left e)
        Right (line0, inp1) -> do
          let line = BS.takeWhile (/= 59) line0
          case parseHex line of
            Nothing -> pure (Left BodyMalformedChunked)
            Just 0  -> do
              eInp <- drainTrailers inp1
              case eInp of
                Left e        -> pure (Left e)
                Right inpDone -> pure (Right (acc, inpDone))
            Just nI
              | nI > toInteger (maxBodyBytes - BS.length acc) -> pure (Left BodyTooLarge)
              | otherwise -> do
                  let n = fromInteger nI :: Int
                  eChunk <- takeExact n inp1
                  case eChunk of
                    Left e -> pure (Left e)
                    Right (chunk, inp2) -> do
                      eCRLF <- readLineCRLF inp2
                      case eCRLF of
                        Left e -> pure (Left e)
                        Right (lineAfterChunk, inp3) ->
                          if BS.null lineAfterChunk
                            then go (acc <> chunk) inp3
                            else pure (Left BodyMalformedChunked)

drainChunked :: Input -> IO (Either BodyError Input)
drainChunked inp0 = go inp0
  where
    go inp = do
      eLine <- readLineCRLF inp
      case eLine of
        Left e -> pure (Left e)
        Right (line0, inp1) -> do
          let line = BS.takeWhile (/= 59) line0
          case parseHex line of
            Nothing -> pure (Left BodyMalformedChunked)
            Just 0  -> drainTrailers inp1
            Just nI
              | nI > toInteger (maxBound :: Int) -> pure (Left BodyTooLarge)
              | otherwise -> do
                  let n = fromInteger nI :: Int
                  eInp2 <- drainExact n inp1
                  case eInp2 of
                    Left e -> pure (Left e)
                    Right inp2 -> do
                      eCRLF <- readLineCRLF inp2
                      case eCRLF of
                        Left e -> pure (Left e)
                        Right (lineAfterChunk, inp3) ->
                          if BS.null lineAfterChunk
                            then go inp3
                            else pure (Left BodyMalformedChunked)

drainTrailers :: Input -> IO (Either BodyError Input)
drainTrailers inp = do
  eLine <- readLineCRLF inp
  case eLine of
    Left e -> pure (Left e)
    Right (line, inp1) ->
      if BS.null line
        then pure (Right inp1)
        else drainTrailers inp1

parseHex :: BS.ByteString -> Maybe Integer
parseHex bs
  | BS.null bs = Nothing
  | otherwise  = BS.foldl' step (Just 0) bs
  where
    step acc w = do
      n <- acc
      d <- hexVal w
      pure (n * 16 + d)

    hexVal :: Word8 -> Maybe Integer
    hexVal w
      | w >= 48 && w <= 57  = Just (toInteger w - 48)
      | w >= 65 && w <= 70  = Just (toInteger w - 55)
      | w >= 97 && w <= 102 = Just (toInteger w - 87)
      | otherwise           = Nothing

maxChunkLineBytes :: Int
maxChunkLineBytes = 8192
