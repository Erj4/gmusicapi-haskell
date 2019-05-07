{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GMusicApi.Types (
  ListData,
  DeviceInfo
) where

import Data.Aeson.Types
import GHC.Generics

newtype ListData a = ListData [a] deriving (Eq, Show)
instance FromJSON a => FromJSON (ListData a) where
  parseJSON = withObject "ListData" $ \json -> do
    jsonData <- json .: "data"
    ListData <$> jsonData .: "items"

data DeviceInfo = DeviceInfo {
  deviceId :: String,
  friendlyName :: String,
  deviceType :: String,
  lastAccessedTimeMs :: String
} deriving (Eq, Show)
instance FromJSON DeviceInfo where
  parseJSON = withObject "DeviceInfo" $ \json -> DeviceInfo
    <$> json .: "id"
    <*> json .: "friendlyName"
    <*> json .: "type"
    <*> json .: "lastAccessedTimeMs"



