{-# LANGUAGE OverloadedStrings #-}
module Http.Parse
  ( requestP
  , connectionPref
  , requireHost
  ) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Http.Types

-- Very strict, subset-friendly parsing:
-- Request-Line: METHOD SP target SP HTTP/x.y CRLF
-- Headers: field-name ":" OWS field-value CRLF
-- End: CRLF
requestP :: A.Parser Request
requestP = do
  m <- methodP <* AC.char ' '
  tgt <- takeTillSP <* AC.char ' '
  ver <- httpVersionP <* crlf
  hs  <- headersP
  _   <- crlf
  pure (Request m tgt ver hs)

methodP :: A.Parser Method
methodP =
      (AC.string "GET"  *> pure GET)
  <|> (AC.string "HEAD" *> pure HEAD)

httpVersionP :: A.Parser BS.ByteString
httpVersionP =
      AC.string "HTTP/1.1"
  <|> AC.string "HTTP/1.0"

headersP :: A.Parser [Header]
headersP = A.many' headerP

headerP :: A.Parser Header
headerP = do
  name <- fieldNameP
  _ <- AC.char ':'
  _ <- ows
  val <- takeTillCR
  crlf
  pure (CI.mk name, val)

fieldNameP :: A.Parser BS.ByteString
fieldNameP = A.takeWhile1 (\c -> c >= 33 && c <= 126 && c /= 58) -- visible ASCII excluding ':'

takeTillSP :: A.Parser BS.ByteString
takeTillSP = A.takeWhile1 (/= 32)

takeTillCR :: A.Parser BS.ByteString
takeTillCR = A.takeWhile (/= 13)

ows :: A.Parser ()
ows = A.skipWhile (\c -> c == 32 || c == 9)

crlf :: A.Parser ()
crlf = AC.string "\r\n" *> pure ()

-- Connection semantics: minimal but honest.
-- HTTP/1.1 defaults keep-alive unless "Connection: close"
-- HTTP/1.0 defaults close unless "Connection: keep-alive"
connectionPref :: Request -> ConnectionPref
connectionPref req =
  case rqVersion req of
    "HTTP/1.1" ->
      case headerLookup "Connection" (rqHeaders req) of
        Just v | hasTokenCI "close" v -> Close
        _                              -> KeepAlive
    "HTTP/1.0" ->
      case headerLookup "Connection" (rqHeaders req) of
        Just v | hasTokenCI "keep-alive" v -> KeepAlive
        _                                   -> Close
    _ -> Close

-- Host is required by HTTP/1.1 (for requests in general); keep it strict.
requireHost :: Request -> Bool
requireHost req =
  rqVersion req /= "HTTP/1.1" || headerLookup "Host" (rqHeaders req) /= Nothing

-- Token matching: cheap + good enough (comma-separated, case-insensitive)
hasTokenCI :: BS.ByteString -> BS.ByteString -> Bool
hasTokenCI tok raw =
  let toks = map (CI.mk . BS.dropWhile (== 32) . BS.takeWhile (/= 32)) (BS.split 44 raw) -- split ','
  in CI.mk tok `elem` toks

