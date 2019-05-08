# TODO

## Types
### URLObject
```hs
url :: URI
```

### Track
```hs
title :: Text,
artist :: Text,
album :: Text,
albumArtist :: Text, --^ may be blank
trackNumber :: Integer,
totalTrackCount :: Maybe Integer,
durationMs :: Integer,
albumArtRef :: Maybe [URLObject], -- Network.URI.JSON@network-uri-json
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
lastRatingChangeTimestamp :: Maybe ZonedTime,
primaryVideo :: Video,
lastModifiedTimestamp :: Maybe ZonedTime,
explicitType :: Maybe Text,
contentType :: Maybe Text,
deleted :: Maybe Bool,
creationTimestamp :: Maybe ZonedTime,
comment :: Maybe Text,
beatsPerMinute :: Maybe Integer,
recentTimestamp :: Maybe ZonedTime,
clientId :: Maybe Text,
id :: Maybe Text
```

### Playlist
```hs
name :: Text,
deleted :: Maybe Boolean,
objectType :: Maybe PlaylistType,
lastModifiedTimestamp :: Maybe ZonedTime,
shareToken :: Text,
ownerProfilePhotoUrl :: Maybe URI,
ownerName :: Maybe Text,
accessControlled :: Maybe Bool,
shareState :: Maybe ShareState,
creationTimestamp :: Maybe ZonedTime,
id :: Maybe Text,
albumArtRef :: Maybe [URLObject],
description :: Maybe Text,
explicitType :: Maybe Text,
contentType :: Maybe Text
```

### PlaylistEntry
```hs
id :: Text,
clientId :: Text,
playlistId :: Text,
absolutePosition :: Text,
trackId :: Text,
creationTimestamp :: ZonedTime,
lastModifiedTimestamp :: ZonedTime,
deleted :: Bool,
source :: Text,
track :: Maybe Track
```

### Attribution
```hs
licenseUrl :: Maybe URI,
licenseTitle :: Maybe Text,
sourceTitle :: Maybe Text,
sourceUrl :: Maybe URI
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2} )
```

### PlaylistType
`PlaylistType = MagicPlaylist | SharedPlaylist | UserGeneratedPlaylist`
Like:
```hs
instance FromJSON Privacy where
  parseJSON (String s) =  pure $ mkPrivacy s
  parseJSON _ = fail "Failed to parse Privacy object"

instance ToJSON Privacy where
  toJSON Everyone = "EVERYONE"
  toJSON AllFriends = "ALL_FRIENDS"
  toJSON FriendsOfFriends = "FRIENDS_OF_FRIENDS"
  toJSON Self = "SELF"
```

### ShareState
`ShareState = Public | Private`
See [PlaylistType](#PlaylistType)

### ImageColorStyles
```hs
primary :: Color,
scrim :: Color,
accent :: Color
```

### Color
```hs
red :: Integer,
green :: Integer,
blue :: Integer
```

### Image
```hs
url :: URI,
aspectRatio :: Maybe Text,
autogen :: Maybe Bool,
colorStyles :: ImageColorStyles
```

### Video
```hs
id :: Text,
title :: Maybe Text,
thumbnails :: Maybe [Thumbnail]
```

### Thumbnail
```hs
url :: URI,
width :: Integer,
height :: Integer
```

### Album
```hs
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
```
- Needs manual FromJSON

### Artist
```hs
name :: Text,
artistArtRef :: Maybe URI,
artistArtRefs :: Maybe [Image],
artistBio :: Maybe Text,
artistId :: Maybe Text,
albums :: Maybe [Album],
topTracks :: Maybe [Track],
totalAlbums :: Maybe Integer, -- from total_albums
artistBioAttribution :: Maybe Attribution, -- from artist_bio_attribution
relatedArtists :: Maybe [Artist] -- from related_artists
```

### Genre
```hs
id :: Maybe Text,
name :: Maybe Text,
children :: Maybe [Text], -- or Text?
parentId :: Maybe Text,
images :: Maybe [URLObject]
```

### StationMetadataSeed
```hs
StationMetadataSeed = ArtistMetadataSeed {artist :: Artist} | GenreMetadataSeed {genre :: Genre}
```

### StationSeed
```hs
StationSeed = AlbumSeed  {albumId  :: Text}
            | ArtistSeed {artistId :: Text, metadataSeed :: ArtistMetadataSeed}
            | GenreSeed  {genreId  :: Text, metadataSeed :: GenreMetadataSeed}
            | TrackSeed  {trackId  :: Text},
            | TrackLockerSeed {trackLockerId :: Text},
            | CuratedStationSeed {curatedStationId :: Text}
```

### StationTrack
```hs
type StationTrack = Track --^ Missing "wentryid" field
```

### Station
```hs
imageUrl :: URI,
name :: Text,
deleted :: Maybe Bool,
lastModifiedTimestamp :: Maybe ZonedTime,
recentTimestamp :: Maybe ZonedTime,
clientId :: Maybe Text,
sessionToken :: Maybe Text,
-- skipEventHistory :: [???], -- TODO
seed :: StationSeed,
stationSeeds :: [StationSeed],
id :: Maybe Text,
description :: Maybe Text,
tracks :: Maybe [Track],
imageUrls :: Maybe [Image],
compositeArtRefs :: Maybe [Image],
contentTypes :: Maybe [Text],
byline :: Maybe Text,
adTargeting :: Maybe AdTargeting
```

### AdTargeting
```hs
keyword :: [Text]
```

### ListenNowAlbum
```hs
artistMetajamId :: Text, -- from _
artistName :: Text, -- from _
artistProfileImage :: URLObject, -- from _
description :: Text,
descriptionAttribution :: Maybe Attribution, -- from _
explicitType :: Maybe Text,
id :: ListenNowAlbumId
```

### ListenNowAlbumId
```hs
metajamCompactKey :: Text,
artist :: Text,
title :: Text
```

### ListenNowStation
```hs
highlightColor :: Maybe Text,
id :: ListenNowStationId,
profileImage :: Maybe URLField
$(deriveJSON...)
```

### ListenNowItem
```hs
compositeArtRefs :: Maybe [Image],
images :: Maybe [Image],
suggestionReason :: Text, -- from suggestion_reason
suggestionText :: Text, -- from suggestion_text
type :: Text,
album :: Maybe ListenNowAlbum,
radioStation :: Maybe ListenNowStation -- from radio_station
```

### PodcastGenre
```hs
id :: Text,
displayName :: Text,
subgroups :: Maybe [Genre]
```

### PodcastEpisode
```hs
art :: Maybe [Image],
author :: Maybe Text,
deleted :: Maybe Text,
description :: Maybe Text,
durationMillis :: Text,
episodeId :: Text,
explicitType :: Text,
fileSize :: Text,
playbackPositionMillis :: Maybe Text,
publicationTimestampMillis :: Maybe Text,
seriesId :: Text,
seriesTitle :: Text,
title :: Text
```

### PodcastSeries
```hs
art :: Maybe [Image],
author :: Text,
continuationToken :: Maybe Text,
copyright :: Maybe Text,
description :: Maybe Text,
episodes :: Maybe [PodcastEpisode],
explicitType :: Text,
link :: Maybe Text,
seriesId :: Text,
title :: Text,
totalNumEpisodes :: Integer,
userPreferences :: PodcastSeriesUserPreferences
```

### PodcastSeriesUserPreferences
```hs
autoDownload :: Maybe Bool,
notifyOnNewEpisode :: Maybe Bool,
subscribed :: Bool
```

### Situation
```hs
description :: Text,
id :: Text,
imageUrl :: Maybe URI,
title :: Text,
wideImageUrl :: Maybe URI,
stations :: Maybe [Station],
situations :: Maybe [Situation]
```

### SearchResultClusterInfo
```hs
category :: Text,
id :: Text,
type :: Text
```

### SearchResult
```hs
score :: Maybe Double,
type :: Text,
bestResult :: Maybe Bool, -- from _
navigationalResult :: Maybe Bool, -- from _
navigationalConfidence :: Maybe Double,
cluster :: Maybe [SearchResultClusterInfo],
artist :: Artist | album :: Album | track :: Track | playlist :: Playlist | genre :: Genre | series :: PodcastSeries | station :: Station | situation :: Situation | youtubeVideo :: Video
$(deriveJSON...)
```

### SearchResultCluster
```hs
cluster :: SearchResultClusterInfo,
displayName :: Maybe Text,
entries :: Maybe [SearchResult],
resultToken :: Maybe Text
```

## API Functions
