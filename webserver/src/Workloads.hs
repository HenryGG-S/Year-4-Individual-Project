{-# LANGUAGE OverloadedStrings #-}
module Workloads
  ( BenchPayloads(..)
  , lenJson1k, lenFile50k, lenFile1m
  , defaultBenchPayloads
  , loadBenchPayloads
  ) where

import Control.Exception (catch, IOException)
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
    , bpFile50k = BS.replicate lenFile50k 120  -- 'x'
    , bpFile1m  = BS.replicate lenFile1m 121   -- 'y'
    }

-- Loads payloads from:
--   <dir>/json1k.json (1024 bytes)
--   <dir>/file50k.bin (51200 bytes)
--   <dir>/file1m.bin  (1048576 bytes)
--
-- If files are missing/wrong size, it generates them (so lean+warp will share the same corpus).
loadBenchPayloads :: FilePath -> IO BenchPayloads
loadBenchPayloads dir = do
  Dir.createDirectoryIfMissing True dir

  let jsonP  = dir </> "json1k.json"
      f50P   = dir </> "file50k.bin"
      f1mP   = dir </> "file1m.bin"

  ensureExact jsonP  mkJson1k  lenJson1k
  ensureRandom f50P  lenFile50k
  ensureRandom f1mP  lenFile1m

  j  <- BS.readFile jsonP
  f50 <- BS.readFile f50P
  f1 <- BS.readFile f1mP

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

ensureExact :: FilePath -> BS.ByteString -> Int -> IO ()
ensureExact p bs n = do
  ok <- fileSizeIs p n
  if ok then pure () else BS.writeFile p bs

ensureRandom :: FilePath -> Int -> IO ()
ensureRandom p n = do
  ok <- fileSizeIs p n
  if ok
    then pure ()
    else do
      -- "arbitrary" bytes without extra deps: read from /dev/urandom
      bytes <- BS.readFile "/dev/urandom" >>= \src ->
        pure (BS.take n src)
      if BS.length bytes == n
        then BS.writeFile p bytes
        else BS.writeFile p (BS.replicate n 90) -- fallback 'Z'

fileSizeIs :: FilePath -> Int -> IO Bool
fileSizeIs p n =
  (do ex <- Dir.doesFileExist p
      if not ex then pure False
      else do sz <- Dir.getFileSize p
              pure (fromIntegral sz == n)
  ) `catch` handler
    where
        handler :: IOException -> IO Bool
        handler _ = pure False
