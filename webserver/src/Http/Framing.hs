{-# LANGUAGE OverloadedStrings #-}
module Http.Framing
  ( BodyFraming(..)
  , FramingError(..)
  , decideBodyFraming
  , hasExpect100
  ) where

import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Data.Word (Word8)
import Http.Types (Header, headerLookupAll)

data BodyFraming
  = NoBody
  | ContentLength !Int
  | Chunked
  deriving (Eq, Show)

data FramingError
  = ConflictingLength
  | InvalidContentLength
  | UnsupportedTransferEncoding
  deriving (Eq, Show)

decideBodyFraming :: [Header] -> Either FramingError BodyFraming
decideBodyFraming hs =
  case (teHeaders, clHeaders) of
    (_:_, _:_) -> Left ConflictingLength
    (_:_, [])  ->
      case parseTransferCodings teHeaders of
        Just codings
          | validChunkedTransferCoding codings -> Right Chunked
          | otherwise                          -> Left UnsupportedTransferEncoding
        Nothing -> Left UnsupportedTransferEncoding
    ([], _:_)  ->
      case parseContentLengths clHeaders of
        Just 0 -> Right NoBody
        Just n -> Right (ContentLength n)
        Nothing -> Left InvalidContentLength
    ([], [])   -> Right NoBody
  where
    teHeaders = headerLookupAll "Transfer-Encoding" hs
    clHeaders = headerLookupAll "Content-Length" hs

hasExpect100 :: [Header] -> Bool
hasExpect100 hs =
  CI.mk "100-continue" `elem` tokens
  where
    tokens =
      [ CI.mk tok
      | raw <- headerLookupAll "Expect" hs
      , tok <- BS.split 44 raw
      , let t = trimOWS tok
      , not (BS.null t)
      ]

validChunkedTransferCoding :: [BS.ByteString] -> Bool
validChunkedTransferCoding toks =
  not (null toks)
    && last cis == CI.mk "chunked"
    && length (filter (== CI.mk "chunked") cis) == 1
  where
    cis = map CI.mk toks

parseTransferCodings :: [BS.ByteString] -> Maybe [BS.ByteString]
parseTransferCodings raws = do
  groups <- traverse splitCommaTokensStrict raws
  let toks = concat groups
  if null toks then Nothing else Just toks

parseContentLengths :: [BS.ByteString] -> Maybe Int
parseContentLengths raws = do
  groups <- traverse splitCommaTokensStrict raws
  let toks = concat groups
  if null toks
    then Nothing
    else do
      ns <- traverse parseStrictContentLength toks
      case ns of
        []     -> Nothing
        n : ns'
          | all (== n) ns' -> Just n
          | otherwise      -> Nothing

parseStrictContentLength :: BS.ByteString -> Maybe Int
parseStrictContentLength raw = do
  let bs = trimOWS raw
  if BS.null bs || BS.any (not . isDigitWord8) bs
    then Nothing
    else decimalToInt bs

splitCommaTokensStrict :: BS.ByteString -> Maybe [BS.ByteString]
splitCommaTokensStrict raw =
  let toks = map trimOWS (BS.split 44 raw)
  in if null toks || any BS.null toks
       then Nothing
       else Just toks

trimOWS :: BS.ByteString -> BS.ByteString
trimOWS = dropStart . dropEnd
  where
    isOWS c = c == 32 || c == 9
    dropStart = BS.dropWhile isOWS
    dropEnd b = BS.reverse (BS.dropWhile isOWS (BS.reverse b))

isDigitWord8 :: Word8 -> Bool
isDigitWord8 w = w >= 48 && w <= 57

decimalToInt :: BS.ByteString -> Maybe Int
decimalToInt bs = fmap fromInteger (go 0 (BS.unpack bs))
  where
    maxI = toInteger (maxBound :: Int)

    go acc [] = Just acc
    go acc (w:ws)
      | not (isDigitWord8 w) = Nothing
      | otherwise =
          let digit = toInteger (fromIntegral w - 48 :: Int)
              acc'  = acc * 10 + digit
          in if acc' > maxI then Nothing else go acc' ws
