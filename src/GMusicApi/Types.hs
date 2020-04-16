{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GMusicApi.Types where

import Data.Aeson.Types
import Data.Aeson.TH(deriveJSON)
import Data.Text(Text)
import Foreign.C.Types(CTime)
import GHC.Generics
import URI.ByteString
import URI.ByteString.Aeson

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

data URLObject = URLObject {
  url :: URIRef Absolute
} deriving (Eq, Show)
$(deriveJSON renameTypeOptions ''URLObject)

data Color = Color {
  red :: Integer,
  green :: Integer,
  blue :: Integer
} deriving (Eq, Show)
$(deriveJSON renameTypeOptions ''Color)

data ImageColorStyles = ImageColorStyles {
  primary :: Color,
  scrim :: Color,
  accent :: Color
} deriving (Eq, Show)
$(deriveJSON renameTypeOptions ''ImageColorStyles)

data Image = Image {
  url :: URIRef Absolute,
  aspectRatio :: Maybe Text,
  autogen :: Maybe Bool,
  colorStyles :: ImageColorStyles
} deriving (Eq, Show)
$(deriveJSON renameTypeOptions ''Image)

data Thumbnail = Thumbnail {
  url :: URIRef Absolute,
  width :: Integer,
  height :: Integer
} deriving (Eq, Show)
$(deriveJSON renameTypeOptions ''Thumbnail)

data Video = Video {
  id :: Text,
  title :: Maybe Text,
  thumbnails :: Maybe [Thumbnail]
} deriving (Eq, Show)
$(deriveJSON renameTypeOptions ''Video)

data Attribution = Attribution {
  licenseUrl :: Maybe URI,
  licenseTitle :: Maybe Text,
  sourceTitle :: Maybe Text,
  sourceUrl :: Maybe URI
} deriving (Eq, Show)
$(deriveJSON renameTypeOptions{fieldLabelModifier = camelTo2 '_'} ''Attribution)

data Track = Track {
  id :: Maybe Text,
  title :: Text,
  artist :: Text,
  album :: Text,
  albumArtist :: Text, -- ^ may be blank
  trackNumber :: Integer,
  totalTrackCount :: Maybe Integer,
  durationMs :: Integer,
  albumArtRef :: Maybe [URLObject],
  artistArtRef :: Maybe [URLObject],
  discNumber :: Integer,
  totalDiscCount :: Maybe Integer,
  estimatedSize :: Maybe Text,
  trackType :: Maybe Text,
  storeId :: Maybe Text,
  albumId :: Text,
  artistId :: Maybe [Text],
  nid :: Maybe Text,
  trackAvailableForPurchase :: Maybe Bool,
  albumAvailableForPurchase :: Maybe Bool,
  composer :: Text,
  playCount :: Maybe Integer,
  year :: Maybe Integer,
  rating :: Maybe Text,
  genre :: Maybe Text,
  trackAvailableForSubscription :: Maybe Bool,
  lastRatingChangeTimestamp :: Maybe CTime,
  primaryVideo :: Video,
  lastModifiedTimestamp :: Maybe CTime,
  explicitType :: Maybe Text,
  contentType :: Maybe Text,
  deleted :: Maybe Bool,
  creationTimestamp :: Maybe CTime,
  comment :: Maybe Text,
  beatsPerMinute :: Maybe Integer,
  recentTimestamp :: Maybe CTime,
  clientId :: Maybe Text
} deriving (Eq, Show)
$(deriveJSON renameTypeOptions ''Track)

data Album = Album {
  name :: Text,
  albumArtist :: Text,
  albumArtRef :: Maybe URI,
  albumId :: Text,
  artist :: Maybe Text,
  artistId :: [Text],
  year :: Integer,
  tracks :: Maybe [Track],
  description :: Maybe Text,
  descriptionAttribution :: Maybe Attribution, -- from description_attribution
  explicitType :: Maybe Text,
  contentType :: Maybe Text
} deriving (Eq, Show)
$(deriveJSON (renameDescriptionAttribution renameTypeOptions) ''Album)

data PlaylistType = MagicPlaylist | SharedPlayList | UserGeneratedPlaylist
  deriving (Eq, Show)

instance FromJSON PlaylistType where
  parseJSON "MAGIC" = pure MagicPlaylist
  parseJSON "SHARED" = pure SharedPlayList
  parseJSON "USER_GENERATED" = pure UserGeneratedPlaylist
  parseJSON _ = fail "Unknown playlist type"

instance ToJSON PlaylistType where
  toJSON MagicPlaylist = "MAGIC"
  toJSON SharedPlayList = "SHARED"
  toJSON UserGeneratedPlaylist = "USER_GENERATED"

data ShareState = Public | Private
  deriving (Eq, Show)

instance FromJSON ShareState where
  parseJSON "PUBLIC" = pure Public
  parseJSON "PRIVATE" = pure Private
  parseJSON _ = fail "Unknown share state"

instance ToJSON ShareState where
  toJSON Public = "PUBLIC"
  toJSON Private = "PRIVATE"

data Playlist = Playlist {
  id :: Maybe Text,
  name :: Text,
  deleted :: Maybe Bool,
  objectType :: Maybe PlaylistType,
  lastModifiedTimestamp :: Maybe CTime,
  shareToken :: Text,
  ownerProfilePhotoUrl :: Maybe URI,
  ownerName :: Maybe Text,
  accessControlled :: Maybe Bool,
  shareState :: Maybe ShareState,
  creationTimestamp :: Maybe CTime,
  albumArtRef :: Maybe [URLObject],
  description :: Maybe Text,
  explicitType :: Maybe Text,
  contentType :: Maybe Text
} deriving (Eq, Show)
$(deriveJSON renameTypeOptions ''Playlist)

data PlaylistEntry = PlaylistEntry {
  id :: Text,
  clientId :: Text,
  playlistId :: Text,
  absolutePosition :: Text,
  trackId :: Text,
  creationTimestamp :: CTime,
  lastModifiedTimestamp :: CTime,
  deleted :: Bool,
  source :: Text,
  track :: Maybe Track
} deriving (Eq, Show)
$(deriveJSON renameTypeOptions ''PlaylistEntry)
