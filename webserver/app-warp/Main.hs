{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (run)

import qualified Workloads

main :: IO ()
main = do
  bench <- Workloads.loadBenchPayloads "bench_files"
  putStrLn "Warp baseline on :8081"
  run 8081 (app bench)

app :: Workloads.BenchPayloads -> Application
app bench req respond =
  case pathInfo req of
    [] ->
      respond $ responseLBS status200 [("Content-Type","text/plain; charset=utf-8")] "ok\n"
    ["health"] ->
      respond $ responseLBS status200 [("Content-Type","text/plain; charset=utf-8")] "healthy\n"
    ["json"] ->
      respond $ responseLBS status200 [("Content-Type","application/json")]
        (LBS.fromStrict (Workloads.bpJson1k bench))
    ["file50k"] ->
      respond $ responseLBS status200 [("Content-Type","application/octet-stream")]
        (LBS.fromStrict (Workloads.bpFile50k bench))
    ["file1m"] ->
      respond $ responseLBS status200 [("Content-Type","application/octet-stream")]
        (LBS.fromStrict (Workloads.bpFile1m bench))
    _ ->
      respond $ responseLBS status404 [("Content-Type","text/plain; charset=utf-8")] "not found\n"
