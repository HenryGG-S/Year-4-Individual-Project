{-# LANGUAGE OverloadedStrings #-}
module Http.Parse
  ( requestHeadP
  , connectionPref
  , requireHost
  , normalisedPath
  , TargetView(..)
  , targetView
  , validHostValue
  , validConnectAuthority
  ) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Data.Word (Word8)
import Http.Types

maxHeaders :: Int
maxHeaders = 100

data TargetView
  = TargetOrigin !BS.ByteString
  | TargetAbsolute !BS.ByteString !BS.ByteString
  | TargetAuthority !BS.ByteString
  | TargetAsterisk
  | TargetInvalid
  deriving (Eq, Show)

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
        Just 13 -> pure (reverse acc)
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
fieldNameP = A.takeWhile1 isTchar

isTchar :: Word8 -> Bool
isTchar w =
     isAlphaNum w
  || w == 33
  || w == 35
  || w == 36
  || w == 37
  || w == 38
  || w == 39
  || w == 42
  || w == 43
  || w == 45
  || w == 46
  || w == 94
  || w == 95
  || w == 96
  || w == 124
  || w == 126

isAlphaNum :: Word8 -> Bool
isAlphaNum w =
     (w >= 48 && w <= 57)
  || (w >= 65 && w <= 90)
  || (w >= 97 && w <= 122)

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
  let raws = headerLookupAll "Connection" (rhHeaders req)
      hasClose = any (hasTokenCI "close") raws
      hasKeepAlive = any (hasTokenCI "keep-alive") raws
  in case rhVersion req of
       "HTTP/1.1"
         | hasClose   -> Close
         | otherwise  -> KeepAlive
       "HTTP/1.0"
         | hasKeepAlive -> KeepAlive
         | otherwise    -> Close
       _ -> Close

requireHost :: RequestHead -> Bool
requireHost req
  | rhVersion req /= "HTTP/1.1" = True
  | otherwise =
      case headerLookupAll "Host" (rhHeaders req) of
        [h] -> validHostValue h
        _   -> False

hasTokenCI :: BS.ByteString -> BS.ByteString -> Bool
hasTokenCI tok raw =
  let toks = map (CI.mk . trimOWS . takeToken) (BS.split 44 raw)
  in CI.mk tok `elem` toks
  where
    takeToken = BS.takeWhile (\c -> c /= 32 && c /= 9)

targetView :: BS.ByteString -> TargetView
targetView tgt
  | BS.null tgt     = TargetInvalid
  | BS.elem 35 tgt  = TargetInvalid
  | tgt == "*"      = TargetAsterisk
  | BS.isPrefixOf "/" tgt = TargetOrigin tgt
  | Just (auth, p) <- parseAbsoluteForm tgt = TargetAbsolute auth p
  | validAuthority tgt = TargetAuthority tgt
  | otherwise = TargetInvalid

normalisedPath :: BS.ByteString -> BS.ByteString
normalisedPath tgt =
  case targetView tgt of
    TargetOrigin p     -> p
    TargetAbsolute _ p -> p
    TargetAsterisk     -> "*"
    _                  -> ""

validHostValue :: BS.ByteString -> Bool
validHostValue raw =
  case parseHostPort (trimOWS raw) of
    Just _  -> True
    Nothing -> False

validConnectAuthority :: BS.ByteString -> Bool
validConnectAuthority raw =
  case parseHostPort (trimOWS raw) of
    Just (_, Just _) -> True
    _                -> False

validAuthority :: BS.ByteString -> Bool
validAuthority = validHostValue

parseAbsoluteForm :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
parseAbsoluteForm tgt
  | Just rest <- BS.stripPrefix "http://" tgt  = parseAbsoluteAfterScheme rest
  | Just rest <- BS.stripPrefix "https://" tgt = parseAbsoluteAfterScheme rest
  | otherwise = Nothing

parseAbsoluteAfterScheme :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
parseAbsoluteAfterScheme rest =
  let (auth, suffix) = BS.break isAuthorityTerminator rest
  in if BS.null auth || not (validAuthority auth)
       then Nothing
       else case BS.uncons suffix of
              Nothing        -> Just (auth, "/")
              Just (47, _)   -> Just (auth, suffix)
              Just (63, xs)  -> Just (auth, "/?" <> xs)
              _              -> Nothing
  where
    isAuthorityTerminator c = c == 47 || c == 63 || c == 35

parseHostPort :: BS.ByteString -> Maybe (BS.ByteString, Maybe Int)
parseHostPort bs
  | BS.null bs     = Nothing
  | BS.elem 64 bs  = Nothing
  | BS.head bs == 91 = parseBracketedHost bs
  | BS.count 58 bs > 1 = Nothing
  | otherwise = parseRegNameOrIPv4 bs

parseBracketedHost :: BS.ByteString -> Maybe (BS.ByteString, Maybe Int)
parseBracketedHost bs = do
  let inner = BS.tail bs
      (lit, rest) = BS.break (== 93) inner
  if BS.null lit || not (validIpLiteral lit)
    then Nothing
    else case BS.uncons rest of
           Just (93, afterBracket) -> do
             port <- parsePortSuffix afterBracket
             pure (lit, port)
           _ -> Nothing

parseRegNameOrIPv4 :: BS.ByteString -> Maybe (BS.ByteString, Maybe Int)
parseRegNameOrIPv4 bs = do
  let (host, rest) = BS.break (== 58) bs
  if not (validRegName host)
    then Nothing
    else do
      port <- parsePortSuffix rest
      pure (host, port)

parsePortSuffix :: BS.ByteString -> Maybe (Maybe Int)
parsePortSuffix rest
  | BS.null rest = Just Nothing
  | otherwise = do
      digits <- BS.stripPrefix ":" rest
      n <- parsePort digits
      pure (Just n)

parsePort :: BS.ByteString -> Maybe Int
parsePort bs
  | BS.null bs = Nothing
  | BS.any (not . isDigitWord8) bs = Nothing
  | otherwise =
      let n = bsToInt bs
      in if n <= 65535 then Just n else Nothing

bsToInt :: BS.ByteString -> Int
bsToInt = BS.foldl' (\acc w -> acc * 10 + fromIntegral (w - 48)) 0

isDigitWord8 :: Word8 -> Bool
isDigitWord8 w = w >= 48 && w <= 57

validRegName :: BS.ByteString -> Bool
validRegName h =
  not (BS.null h) && BS.all isRegNameChar h

isRegNameChar :: Word8 -> Bool
isRegNameChar w =
     isAlphaNum w
  || w == 45
  || w == 46

validIpLiteral :: BS.ByteString -> Bool
validIpLiteral h =
  not (BS.null h) && BS.all isIpLiteralChar h

isIpLiteralChar :: Word8 -> Bool
isIpLiteralChar w =
     isAlphaNum w
  || w == 58
  || w == 46
  || w == 45

trimOWS :: BS.ByteString -> BS.ByteString
trimOWS = dropStart . dropEnd
  where
    isOWS c = c == 32 || c == 9
    dropStart = BS.dropWhile isOWS
    dropEnd b = BS.reverse (BS.dropWhile isOWS (BS.reverse b))
