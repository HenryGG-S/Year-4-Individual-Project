{-# LANGUAGE OverloadedStrings #-}
module Http.Framing
  ( BodyFraming(..)
  , FramingError(..)
  , decideBodyFraming
  , hasExpect100
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import Http.Types (Header, headerLookup)

data BodyFraming
  = NoBody
  | ContentLength !Int
  | Chunked
  deriving (Eq, Show)

data FramingError
  = ConflictingLength   -- TE and CL both present
  | InvalidContentLength
  | UnsupportedTransferEncoding
  deriving (Eq, Show)

-- RFC-ish rules (pragmatic + safe):
-- - If TE and CL both present => reject (smuggling risk)
-- - If TE present => must end in "chunked" => Chunked, else reject
-- - Else if CL present => parse (allow "0" or comma-list with same value)
-- - Else => NoBody
decideBodyFraming :: [Header] -> Either FramingError BodyFraming
decideBodyFraming hs =
  case (teHeader, clHeader) of
    (Just _, Just _) -> Left ConflictingLength
    (Just te, Nothing) ->
      if teEndsInChunked te then Right Chunked else Left UnsupportedTransferEncoding
    (Nothing, Just cl) ->
      case parseContentLength cl of
        Just n | n >= 0    -> Right (if n == 0 then NoBody else ContentLength n)
        _                  -> Left InvalidContentLength
    (Nothing, Nothing) -> Right NoBody
  where
    teHeader = headerLookup "Transfer-Encoding" hs
    clHeader = headerLookup "Content-Length" hs

hasExpect100 :: [Header] -> Bool
hasExpect100 hs =
  case headerLookup "Expect" hs of
    Nothing -> False
    Just v  ->
      let toks = map (CI.mk . trimOWS) (BS.split 44 v) -- comma
      in CI.mk "100-continue" `elem` toks

-- Transfer-Encoding is a list of codings; chunked must be final coding.
teEndsInChunked :: BS.ByteString -> Bool
teEndsInChunked raw =
  let parts = map (CI.mk . trimOWS) (BS.split 44 raw)
  in not (null parts) && last parts == CI.mk "chunked"

parseContentLength :: BS.ByteString -> Maybe Int
parseContentLength raw =
  case map trimOWS (BS.split 44 raw) of
    [] -> Nothing
    xs ->
      let ms = map parseInt xs
      in case ms of
           [] -> Nothing
           (Just n : rest) | all (== Just n) rest -> Just n
           _                                      -> Nothing
  where
    parseInt bs = case B8.readInt (B8.pack (B8.unpack bs)) of
      Just (n, _) -> Just n
      Nothing     -> Nothing

trimOWS :: BS.ByteString -> BS.ByteString
trimOWS =
  dropStart . dropEnd
  where
    isOWS c = c == 32 || c == 9
    dropStart = BS.dropWhile isOWS
    dropEnd b = BS.reverse (BS.dropWhile isOWS (BS.reverse b))
