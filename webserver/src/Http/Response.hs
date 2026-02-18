{-# LANGUAGE OverloadedStrings #-}
module Http.Response
  ( Status(..), ok, badRequest, notFound, methodNotAllowed, requestHeaderFieldsTooLarge
  , mkResponse
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB

data Status = Status !Int !BS.ByteString

ok, badRequest, notFound, methodNotAllowed :: Status
ok               = Status 200 "OK"
badRequest       = Status 400 "Bad Request"
notFound         = Status 404 "Not Found"
methodNotAllowed = Status 405 "Method Not Allowed"

-- | Build an HTTP/1.1 response.
-- The caller supplies headers (including Connection) so the server can decide
-- keep-alive vs close per request.
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
        ]
      hdrs =
        concatMap
          (\(k,v) -> [BB.byteString k, ": ", BB.byteString v, "\r\n"])
          headers
      end = ["\r\n"]
      payload = if sendBody then [BB.byteString body] else []
  in mconcat (base ++ hdrs ++ end ++ payload)

requestHeaderFieldsTooLarge :: Status
requestHeaderFieldsTooLarge = Status 431 "Request Header Fields Too Large"

