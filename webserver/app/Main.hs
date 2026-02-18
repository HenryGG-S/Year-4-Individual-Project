module Main where

import Server (runServer)

main :: IO ()
main = runServer "8080"

