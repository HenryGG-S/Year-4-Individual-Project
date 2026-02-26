{-# LANGUAGE OverloadedStrings #-}
module Http.Types
  ( Method(..)
  , RequestHead(..)
  , Header
  , headerLookup
  , ConnectionPref(..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI

data Method
  = GET | HEAD | POST | PUT | DELETE | OPTIONS | PATCH | TRACE | CONNECT
  | Other !BS.ByteString
  deriving (Eq, Show)

type Header = (CI.CI BS.ByteString, BS.ByteString)

data RequestHead = RequestHead
  { rhMethod  :: !Method
  , rhTarget  :: !BS.ByteString  -- raw request-target (we’ll normalise in routing)
  , rhVersion :: !BS.ByteString
  , rhHeaders :: ![Header]
  } deriving (Show)

data ConnectionPref = KeepAlive | Close deriving (Eq, Show)

headerLookup :: BS.ByteString -> [Header] -> Maybe BS.ByteString
headerLookup name hs = lookup (CI.mk name) hs
