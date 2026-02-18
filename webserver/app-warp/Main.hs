{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (run)
import Workloads (json1k, file50k, file1m)

main :: IO ()
main = do
  putStrLn "Warp baseline on :8081"
  run 8081 app

app :: Application
app req respond = do
  let p = pathInfo req
  case p of
    [] -> respond $ responseLBS status200 [("Content-Type","text/plain; charset=utf-8")] "ok\n"
    ["health"] -> respond $ responseLBS status200 [("Content-Type","text/plain; charset=utf-8")] "healthy\n"
    ["json"] ->
      respond $ responseLBS status200 [("Content-Type","application/json")] (LBS.fromStrict json1k)
    ["file50k"] ->
      respond $ responseLBS status200 [("Content-Type","application/octet-stream")] (LBS.fromStrict file50k)
    ["file1m"] ->
      respond $ responseLBS status200 [("Content-Type","application/octet-stream")] (LBS.fromStrict file1m)
    _ -> respond $ responseLBS status404 [("Content-Type","text/plain; charset=utf-8")] "not found\n"

