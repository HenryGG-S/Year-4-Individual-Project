{-# LANGUAGE OverloadedStrings #-}
module Workloads
  ( json1k
  , file50k
  , file1m
  , lenJson1k
  , lenFile50k
  , lenFile1m
  ) where

import qualified Data.ByteString as BS

lenJson1k, lenFile50k, lenFile1m :: Int
lenJson1k  = 1024
lenFile50k = 50 * 1024
lenFile1m  = 1024 * 1024

-- Valid JSON padded to exactly 1024 bytes.
json1k :: BS.ByteString
json1k =
  let prefix = "{\"ok\":true,\"pad\":\""
      suffix = "\"}\n"
      padLen = lenJson1k - (BS.length prefix + BS.length suffix)
      pad    = BS.replicate (max 0 padLen) 97 -- 'a'
  in prefix <> pad <> suffix

file50k :: BS.ByteString
file50k = BS.replicate lenFile50k 120 -- 'x'

file1m :: BS.ByteString
file1m = BS.replicate lenFile1m 121 -- 'y'

