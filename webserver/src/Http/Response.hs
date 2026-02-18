{-# LANGUAGE OverloadedStrings #-}
module Http.Response
  ( Status(..), ok, badRequest, notFound, methodNotAllowed
  , mkResponse
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as B8

data Status = Status !Int !BS.ByteString

ok, badRequest, notFound, methodNotAllowed :: Status
ok               = Status 200 "OK"
badRequest       = Status 400 "Bad Request"
notFound         = Status 404 "Not Found"
methodNotAllowed = Status 405 "Method Not Allowed"

mkResponse :: Status -> [(BS.ByteString, BS.ByteString)] -> BS.ByteString -> Bool -> BB.Builder
mkResponse (Status code msg) headers body sendBody =
  let len = BS.length body
      base =
        [ "HTTP/1.1 "
        , BB.intDec code
        , " "
        , BB.byteString msg
        , "\r\n"
        , "Content-Length: "
        , BB.intDec len
        , "\r\n"
        , "Connection: close\r\n"   -- Slice 1.5: we’ll override later per-request
        ]
      hdrs = concatMap (\(k,v) -> [BB.byteString k, ": ", BB.byteString v, "\r\n"]) headers
  in mconcat (base ++ hdrs ++ ["\r\n"] ++ [if sendBody then BB.byteString body else mempty])

