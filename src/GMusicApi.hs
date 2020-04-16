{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GMusicApi (
  getDataDirectory,
  getDefaultTokenFile,
  getAccessToken,
  getManager,
  getDeviceManagementInfo
) where

import qualified GMusicApi.Types as API

import Control.Exception.Safe (handleIO, throwString)
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString, pack, unpack)
import Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import URI.ByteString
import URI.ByteString.QQ
import Network.OAuth.OAuth2
import System.Directory
import System.FilePath

clientId :: Text
clientId = "228293309116.apps.googleusercontent.com"
clientSecret :: Text
clientSecret = "GL1YV0XMp0RlL7ylCV3ilFz-"
scopes :: [Text]
scopes = ["https://www.googleapis.com/auth/skyjam"]

oauth2 :: OAuth2
oauth2 = OAuth2 {
  oauthClientId = clientId,
  oauthClientSecret = Just clientSecret,
  oauthOAuthorizeEndpoint = appendQueryParams
    [
      ("redirect_uri", "urn:ietf:wg:oauth:2.0:oob"),
      ("scope", encodeUtf8 $ T.intercalate (T.pack " ") scopes)
    ]
    [uri|https://accounts.google.com/o/oauth2/auth|],
  oauthAccessTokenEndpoint = appendQueryParams
    [
      ("redirect_uri", "urn:ietf:wg:oauth:2.0:oob")
    ]
    [uri|https://www.googleapis.com/oauth2/v3/token|],
  oauthCallback = Nothing
}

getManager :: IO Manager
getManager = newManager tlsManagerSettings

createURL :: Text -> Text -> URI
createURL locale = appendURIPath $ appendURIQuery [uri|https://mclients.googleapis.com/sj/v2.5/?dv=0&tier=aa|] ("hl", locale)

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
getAccessToken fp = do
  putStrLn $ "Get verification code from " <> ( C8.unpack $ serializeURIRef' $ authorizationUrl oauth2)
  code <- getLine
  manager <- getManager
  token <- fetchAccessToken manager oauth2 $ ExchangeToken $ T.pack code
  fromEither token
  where
    fromEither :: Show a => Either a b -> IO b
    fromEither = either (throwString . show) pure

appendURIPath :: URI -> Text -> URI
appendURIPath base path = base { uriPath = encodeUtf8 $ T.append basePath path }
  where basePath = decodeUtf8 $ uriPath base

appendURIQuery :: URI -> (Text, Text) -> URI
appendURIQuery base (key, val) = base { uriQuery = Query { queryPairs = query : baseQuery} }
  where
    baseQuery = queryPairs $ uriQuery base
    query = (encodeUtf8 key, encodeUtf8 val)

getDeviceManagementInfo :: Manager -> OAuth2Token -> IO (Either Lazy.ByteString (API.ListData API.DeviceInfo))
getDeviceManagementInfo mgr oauthToken = authGetJSON mgr token $ createURL "en_US" "devicemanagementinfo"
  where token = accessToken oauthToken

--getAllSongs :: FromJSON err => Manager -> OAuth2Token -> IO (OAuth2Result err (ListData Track))
--getAllSongs mgr oauthToken = authGetJSON mgr token $ createURL "en_US"
