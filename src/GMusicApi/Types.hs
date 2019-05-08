{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GMusicApi.Types (
  ListData,
  DeviceInfo
) where

import Data.Aeson.Types
import Data.Aeson.TH(deriveJSON)
import GHC.Generics
import GMusicApi.Types.Utils

newtype ListData a = ListData [a] deriving (Eq, Show)
instance FromJSON a => FromJSON (ListData a) where
  parseJSON = withObject "ListData" $ \json -> do
    jsonData <- json .: "data"
    ListData <$> jsonData .: "items"

data DeviceInfo = DeviceInfo {
  id :: String,
  friendlyName :: String,
  objectType :: String,
  lastAccessedTimeMs :: String
} deriving (Eq, Show)
$(deriveJSON renameTypeOptions ''DeviceInfo)

