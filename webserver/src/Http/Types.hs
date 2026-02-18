{-# LANGUAGE OverloadedStrings #-}
module Http.Types
  ( Method(..), Request(..), Header, headerLookup, ConnectionPref(..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI

data Method
  = GET
  | HEAD
  | Other !BS.ByteString
  deriving (Eq, Show)

type Header = (CI.CI BS.ByteString, BS.ByteString)

data Request = Request
  { rqMethod  :: !Method
  , rqTarget  :: !BS.ByteString
  , rqVersion :: !BS.ByteString  -- "HTTP/1.1" or "HTTP/1.0"
  , rqHeaders :: ![Header]
  } deriving (Show)

data ConnectionPref = KeepAlive | Close deriving (Eq, Show)

headerLookup :: BS.ByteString -> [Header] -> Maybe BS.ByteString
headerLookup name hs = lookup (CI.mk name) hs

