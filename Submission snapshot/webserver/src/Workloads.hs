{-# LANGUAGE OverloadedStrings #-}
module Workloads
  ( BenchPayloads(..)
  , lenJson1k, lenFile50k, lenFile1m
  , defaultBenchPayloads
  , loadBenchPayloads
  ) where

import Control.Exception (IOException, catch)
import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import System.FilePath ((</>))

data BenchPayloads = BenchPayloads
  { bpJson1k  :: !BS.ByteString
  , bpFile50k :: !BS.ByteString
  , bpFile1m  :: !BS.ByteString
  } deriving (Show)

lenJson1k, lenFile50k, lenFile1m :: Int
lenJson1k  = 1024
lenFile50k = 50 * 1024
lenFile1m  = 1024 * 1024

defaultBenchPayloads :: BenchPayloads
defaultBenchPayloads =
  BenchPayloads
    { bpJson1k  = mkJson1k
    , bpFile50k = mkFile50k
    , bpFile1m  = mkFile1m
    }

-- Loads payloads from:
--   <dir>/json1k.json (1024 bytes)
--   <dir>/file50k.bin (51200 bytes)
--   <dir>/file1m.bin  (1048576 bytes)
--
-- If files are missing or differ from the deterministic corpus,
-- they are rewritten so lean + warp share the same exact bytes.
loadBenchPayloads :: FilePath -> IO BenchPayloads
loadBenchPayloads dir = do
  Dir.createDirectoryIfMissing True dir

  let jsonP = dir </> "json1k.json"
      f50P  = dir </> "file50k.bin"
      f1mP  = dir </> "file1m.bin"

  ensureExactBytes jsonP mkJson1k
  ensureExactBytes f50P  mkFile50k
  ensureExactBytes f1mP  mkFile1m

  j   <- BS.readFile jsonP
  f50 <- BS.readFile f50P
  f1  <- BS.readFile f1mP

  pure BenchPayloads
    { bpJson1k  = j
    , bpFile50k = f50
    , bpFile1m  = f1
    }

-- ===== helpers =====

mkJson1k :: BS.ByteString
mkJson1k =
  let prefix = "{\"ok\":true,\"pad\":\""
      suffix = "\"}\n"
      padLen = lenJson1k - (BS.length prefix + BS.length suffix)
      pad    = BS.replicate (max 0 padLen) 97 -- 'a'
  in prefix <> pad <> suffix

mkFile50k :: BS.ByteString
mkFile50k =
  mkRepeatTo lenFile50k
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

mkFile1m :: BS.ByteString
mkFile1m =
  mkRepeatTo lenFile1m
    "0123456789abcdefFEDCBA9876543210~!@#$%^&*()[]{}<>?/|+=-_:;,. "

mkRepeatTo :: Int -> BS.ByteString -> BS.ByteString
mkRepeatTo n pat
  | n <= 0        = BS.empty
  | BS.null pat   = BS.replicate n 0
  | otherwise     = BS.take n (BS.concat (replicate reps pat))
  where
    reps = (n `div` BS.length pat) + 1

ensureExactBytes :: FilePath -> BS.ByteString -> IO ()
ensureExactBytes p bs = do
  same <- fileBytesEqual p bs
  if same then pure () else BS.writeFile p bs

fileBytesEqual :: FilePath -> BS.ByteString -> IO Bool
fileBytesEqual p expected =
  (do ex <- Dir.doesFileExist p
      if not ex
        then pure False
        else do
          sz <- Dir.getFileSize p
          if fromIntegral sz /= BS.length expected
            then pure False
            else do
              actual <- BS.readFile p
              pure (actual == expected)
  ) `catch` handler
  where
    handler :: IOException -> IO Bool
    handler _ = pure False
