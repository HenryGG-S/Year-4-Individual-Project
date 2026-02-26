{-# LANGUAGE OverloadedStrings #-}
module Http.Response
  ( Status(..)
  , ok, created, noContent
  , badRequest, notFound, conflict
  , methodNotAllowed, notImplemented
  , lengthRequired, payloadTooLarge
  , internalServerError
  , requestHeaderFieldsTooLarge
  , mkResponse
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB

data Status = Status !Int !BS.ByteString

ok, created, noContent :: Status
ok        = Status 200 "OK"
created   = Status 201 "Created"
noContent = Status 204 "No Content"

badRequest, notFound, conflict :: Status
badRequest = Status 400 "Bad Request"
notFound   = Status 404 "Not Found"
conflict   = Status 409 "Conflict"

methodNotAllowed, notImplemented :: Status
methodNotAllowed = Status 405 "Method Not Allowed"
notImplemented   = Status 501 "Not Implemented"

lengthRequired, payloadTooLarge :: Status
lengthRequired  = Status 411 "Length Required"
payloadTooLarge = Status 413 "Payload Too Large"

internalServerError :: Status
internalServerError = Status 500 "Internal Server Error"

requestHeaderFieldsTooLarge :: Status
requestHeaderFieldsTooLarge = Status 431 "Request Header Fields Too Large"

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
