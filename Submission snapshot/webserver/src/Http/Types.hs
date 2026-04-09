{-# LANGUAGE OverloadedStrings #-}
module Http.Types
  ( Method(..)
  , RequestHead(..)
  , Header
  , headerLookup
  , headerLookupAll
  , headerCount
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
  , rhTarget  :: !BS.ByteString
  , rhVersion :: !BS.ByteString
  , rhHeaders :: ![Header]
  } deriving (Show)

data ConnectionPref = KeepAlive | Close deriving (Eq, Show)

headerLookup :: BS.ByteString -> [Header] -> Maybe BS.ByteString
headerLookup name hs =
  case headerLookupAll name hs of
    []    -> Nothing
    v : _ -> Just v

headerLookupAll :: BS.ByteString -> [Header] -> [BS.ByteString]
headerLookupAll name =
  map snd . filter (\(k, _) -> k == CI.mk name)

headerCount :: BS.ByteString -> [Header] -> Int
headerCount name = length . headerLookupAll name
