module Main where

import Control.Concurrent (forkIO, myThreadId, threadDelay)
import Control.Exception (throwTo)
import Server (runServer)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitSuccess))
import Text.Read (readMaybe)

main :: IO ()
main = do
  mPort <- lookupEnv "PORT"
  mDiagExit <- lookupEnv "DIAG_EXIT_AFTER_SECONDS"

  case mDiagExit >>= readMaybe of
    Just secs | secs > 0 -> do
      mainTid <- myThreadId
      _ <- forkIO $ do
        threadDelay (secs * 1000000)
        throwTo mainTid ExitSuccess
      pure ()
    _ ->
      pure ()

  runServer (maybe "8080" id mPort)
