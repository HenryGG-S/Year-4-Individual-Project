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

-- ===== Config bounds =====

maxHeaders :: Int
maxHeaders = 100

-- ===== Parser =====

requestP :: A.Parser Request
requestP = do
  mTok <- tokenTillSP <* AC.char ' '
  let m = case mTok of
            "GET"  -> GET
            "HEAD" -> HEAD
            _      -> Other mTok
  tgt <- takeTillSP <* AC.char ' '
  ver <- httpVersionP <* crlf
  hs  <- headersP
  _   <- crlf
  pure (Request m tgt ver hs)

httpVersionP :: A.Parser BS.ByteString
httpVersionP =
      AC.string "HTTP/1.1"
  <|> AC.string "HTTP/1.0"

headersP :: A.Parser [Header]
headersP = go 0 []
  where
    go :: Int -> [Header] -> A.Parser [Header]
    go n acc = do
      mw <- A.peekWord8
      case mw of
        Just 13 -> pure (reverse acc)  -- '\r' => CRLF => end of headers
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
fieldNameP = A.takeWhile1 (\c -> c >= 33 && c <= 126 && c /= 58) -- visible ASCII excluding ':'

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

-- ===== Semantics =====

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

requireHost :: Request -> Bool
requireHost req =
  rqVersion req /= "HTTP/1.1" || headerLookup "Host" (rqHeaders req) /= Nothing

-- Comma-separated tokens; trim OWS; case-insensitive compare.
hasTokenCI :: BS.ByteString -> BS.ByteString -> Bool
hasTokenCI tok raw =
  let parts = map trimOWS (BS.split 44 raw) -- ','
      toks  = map (CI.mk . takeToken) parts
  in CI.mk tok `elem` toks
  where
    takeToken = BS.takeWhile (\c -> c /= 32 && c /= 9)
    trimOWS = dropWhileOWS . dropWhileEndOWS
    dropWhileOWS = BS.dropWhile (\c -> c == 32 || c == 9)
    dropWhileEndOWS bs = BS.reverse (BS.dropWhile (\c -> c == 32 || c == 9) (BS.reverse bs))

