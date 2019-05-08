module GMusicApi.Types.Utils (
  renameTypeOptions
) where

import Data.Aeson.TH(defaultOptions, Options(fieldLabelModifier))

renameTypeOptions :: Options
renameTypeOptions = defaultOptions { fieldLabelModifier = renameType} where
  renameType "objectType" = "type"
  renameType name = name
