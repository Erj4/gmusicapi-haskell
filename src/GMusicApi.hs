{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GMusicApi (
  getDataDirectory,
  getDefaultTokenFile,
  getAccessToken,
  manager
) where

import GMusicApi.Types

import Data.Aeson (FromJSON)
import Data.ByteString
import Data.Text (Text)
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import URI.ByteString
import URI.ByteString.QQ
import Network.OAuth.OAuth2 (OAuth2Token, OAuth2Result, accessToken, authGetJSON)
import qualified Network.Google.OAuth2 (getAccessToken)
import System.Directory
import System.FilePath

oauthClientId :: Text
oauthClientId = "228293309116.apps.googleusercontent.com"
oauthClientSecret :: Text
oauthClientSecret = "GL1YV0XMp0RlL7ylCV3ilFz-"
oauthScopes :: [Text]
oauthScopes = ["https://www.googleapis.com/auth/skyjam"]

manager :: IO Manager
manager = newManager tlsManagerSettings

createURL :: ByteString -> ByteString -> URI
createURL locale = appendURIPath $ appendURIQuery [uri|https://mclients.googleapis.com/sj/v2.5/?dv=0&tier=aa|] [("hl", locale)]

-- |Gets the default data directory for gmusicapi-haskell
-- The directory will be created if it does not already exist
getDataDirectory :: IO FilePath
getDataDirectory = do
  dir <- getXdgDirectory XdgData "gmusicapi-haskell"
  createDirectoryIfMissing True dir
  return dir

-- |Gets the default location for the OAuth2 token file
getDefaultTokenFile :: IO FilePath
getDefaultTokenFile = do
  dir <- getDataDirectory 
  return $ dir </> "oauth2token"

-- |Gets OAauth2 access token for Google Play Music API
-- This will be loaded from the given file if present, or otherwise it will be obtained from the Google OAuth2 API
getAccessToken :: Maybe FilePath -- ^FilePath to check for existing token and write new one to
 -> IO OAuth2Token -- ^ OAuth2 token
getAccessToken = Network.Google.OAuth2.getAccessToken oauthClientId oauthClientSecret oauthScopes

appendURIPath :: URI -> ByteString -> URI
appendURIPath base path = base { uriPath = append basePath path }
  where basePath = uriPath base

appendURIQuery :: URI -> [(ByteString, ByteString)] -> URI
appendURIQuery base query = base { uriQuery = Query { queryPairs = baseQuery ++ query} }
  where baseQuery = queryPairs $ uriQuery base

getDeviceManagementInfo :: FromJSON err => Manager -> OAuth2Token -> IO (OAuth2Result err (ListData DeviceInfo))
getDeviceManagementInfo mgr oauthToken = authGetJSON mgr token $ createURL "en_US" "devicemanagementinfo"
  where token = accessToken oauthToken

--getAllSongs :: FromJSON err => Manager -> OAuth2Token -> IO (OAuth2Result err (ListData Track))
--getAllSongs mgr oauthToken = authGetJSON mgr token $ createURL "en_US"
