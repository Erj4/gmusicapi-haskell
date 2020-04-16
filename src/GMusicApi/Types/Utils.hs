module GMusicApi.Types.Utils (
  renameTypeOptions,
  renameDescriptionAttribution
) where

import Data.Aeson.TH(defaultOptions, Options(fieldLabelModifier))

renameTypeOptions :: Options
renameTypeOptions = defaultOptions { fieldLabelModifier = renameType} where
  renameType "objectType" = "type"
  renameType name = name

renameDescriptionAttribution :: Options -> Options
renameDescriptionAttribution options = options { fieldLabelModifier = rename } where
  rename = rename'.(fieldLabelModifier options)
  rename' "descriptionAttribution" = "description_attribution"
  rename' name = name
