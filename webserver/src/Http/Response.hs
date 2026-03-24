{-# LANGUAGE OverloadedStrings #-}
module Http.Response
  ( Status(..)
  , ok, created, noContent, notModified
  , badRequest, notFound, conflict, preconditionFailed
  , methodNotAllowed, notImplemented
  , lengthRequired, payloadTooLarge
  , internalServerError
  , requestHeaderFieldsTooLarge
  , requestTimeout
  , expectationFailed
  , mkResponse
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB

data Status = Status !Int !BS.ByteString

ok, created, noContent, notModified :: Status
ok          = Status 200 "OK"
created     = Status 201 "Created"
noContent   = Status 204 "No Content"
notModified = Status 304 "Not Modified"

badRequest, notFound, conflict :: Status
badRequest = Status 400 "Bad Request"
notFound   = Status 404 "Not Found"
conflict   = Status 409 "Conflict"
preconditionFailed = Status 412 "Precondition Failed"

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

requestTimeout :: Status
requestTimeout = Status 408 "Request Timeout"

expectationFailed :: Status
expectationFailed = Status 417 "Expectation Failed"

mkResponse
  :: BS.ByteString
  -> Status
  -> [(BS.ByteString, BS.ByteString)]
  -> BS.ByteString
  -> Bool
  -> BB.Builder
mkResponse version (Status code msg) headers body sendBody =
  let len = BS.length body
      base =
        [ BB.byteString version
        , " "
        , BB.intDec code
        , " "
        , BB.byteString msg
        , "\r\n"
        ]
      clHdr =
        if statusForbidsContentLength code
          then []
          else
            [ "Content-Length: "
            , BB.intDec len
            , "\r\n"
            ]
      hdrs =
        concatMap
          (\(k, v) -> [BB.byteString k, ": ", BB.byteString v, "\r\n"])
          headers
      end = ["\r\n"]
      payload =
        if sendBody && not (statusForbidsBody code)
          then [BB.byteString body]
          else []
  in mconcat (base ++ clHdr ++ hdrs ++ end ++ payload)

statusForbidsContentLength :: Int -> Bool
statusForbidsContentLength code =
  (code >= 100 && code < 200) || code == 204 || code == 304

statusForbidsBody :: Int -> Bool
statusForbidsBody code =
  (code >= 100 && code < 200) || code == 204 || code == 304
