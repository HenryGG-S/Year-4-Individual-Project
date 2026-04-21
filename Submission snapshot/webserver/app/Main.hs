module Main where

import Server (runServer)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  mPort <- lookupEnv "PORT"
  runServer (maybe "8080" id mPort)
