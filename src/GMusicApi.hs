module GMusicApi (
  getDataDirectory,
  getDefaultTokenFile,
  getAccessToken
) where

import qualified Data.Text as T
import Network.OAuth.OAuth2 (OAuth2Token)
import qualified Network.Google.OAuth2 (getAccessToken)
import System.Directory
import System.FilePath

oauthClientId = T.pack "228293309116.apps.googleusercontent.com"
oauthClientSecret = T.pack "GL1YV0XMp0RlL7ylCV3ilFz-"
oauthScopes = map T.pack ["https://www.googleapis.com/auth/skyjam"]

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
