{-# LANGUAGE OverloadedStrings #-}
module Http.Parse
  ( requestHeadP
  , connectionPref
  , requireHost
  , normalisedPath
  ) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Http.Types

-- ===== bounds =====
maxHeaders :: Int
maxHeaders = 100

requestHeadP :: A.Parser RequestHead
requestHeadP = do
  mTok <- tokenTillSP <* AC.char ' '
  let m = case mTok of
            "GET"     -> GET
            "HEAD"    -> HEAD
            "POST"    -> POST
            "PUT"     -> PUT
            "DELETE"  -> DELETE
            "OPTIONS" -> OPTIONS
            "PATCH"   -> PATCH
            "TRACE"   -> TRACE
            "CONNECT" -> CONNECT
            _         -> Other mTok

  tgt <- takeTillSP <* AC.char ' '
  ver <- httpVersionP <* crlf
  hs  <- headersP
  _   <- crlf
  pure (RequestHead m tgt ver hs)

httpVersionP :: A.Parser BS.ByteString
httpVersionP =
      AC.string "HTTP/1.1"
  <|> AC.string "HTTP/1.0"

headersP :: A.Parser [Header]
headersP = go 0 []
  where
    go n acc = do
      mw <- A.peekWord8
      case mw of
        Just 13 -> pure (reverse acc) -- '\r' => end headers
        _ ->
          if n >= maxHeaders
            then fail "too many headers"
            else do
              h <- headerP
              go (n + 1) (h : acc)

headerP :: A.Parser Header
headerP = do
  name <- fieldNameP
  _ <- AC.char ':'
  _ <- ows
  val <- takeTillCR
  crlf
  pure (CI.mk name, val)

fieldNameP :: A.Parser BS.ByteString
fieldNameP = A.takeWhile1 (\c -> c >= 33 && c <= 126 && c /= 58)

tokenTillSP :: A.Parser BS.ByteString
tokenTillSP = A.takeWhile1 (/= 32)

takeTillSP :: A.Parser BS.ByteString
takeTillSP = A.takeWhile1 (/= 32)

takeTillCR :: A.Parser BS.ByteString
takeTillCR = A.takeWhile (/= 13)

ows :: A.Parser ()
ows = A.skipWhile (\c -> c == 32 || c == 9)

crlf :: A.Parser ()
crlf = AC.string "\r\n" *> pure ()

connectionPref :: RequestHead -> ConnectionPref
connectionPref req =
  case rhVersion req of
    "HTTP/1.1" ->
      case headerLookup "Connection" (rhHeaders req) of
        Just v | hasTokenCI "close" v -> Close
        _                              -> KeepAlive
    "HTTP/1.0" ->
      case headerLookup "Connection" (rhHeaders req) of
        Just v | hasTokenCI "keep-alive" v -> KeepAlive
        _                                   -> Close
    _ -> Close

requireHost :: RequestHead -> Bool
requireHost req =
  rhVersion req /= "HTTP/1.1" || headerLookup "Host" (rhHeaders req) /= Nothing

hasTokenCI :: BS.ByteString -> BS.ByteString -> Bool
hasTokenCI tok raw =
  let toks = map (CI.mk . trimOWS . takeToken) (BS.split 44 raw)
  in CI.mk tok `elem` toks
  where
    takeToken = BS.takeWhile (\c -> c /= 32 && c /= 9)

trimOWS :: BS.ByteString -> BS.ByteString
trimOWS = dropStart . dropEnd
  where
    isOWS c = c == 32 || c == 9
    dropStart = BS.dropWhile isOWS
    dropEnd b = BS.reverse (BS.dropWhile isOWS (BS.reverse b))

-- Minimal target normalisation for origin server:
-- - origin-form "/path?x" => unchanged
-- - absolute-form "http://host/path" => extract "/path" (or "/" if none)
-- - asterisk "*" => "*"
-- - authority-form "host:port" (CONNECT) => "" (route to 501)
normalisedPath :: BS.ByteString -> BS.ByteString
normalisedPath tgt
  | tgt == "*" = "*"
  | BS.isPrefixOf "/" tgt = tgt
  | BS.isPrefixOf "http://" tgt = absToPath (BS.drop 7 tgt)
  | BS.isPrefixOf "https://" tgt = absToPath (BS.drop 8 tgt)
  | otherwise = ""  -- authority-form or garbage
  where
    absToPath rest =
      case BS.break (== 47) rest of -- '/'
        (_auth, p)
          | BS.null p  -> "/"
          | otherwise  -> p
