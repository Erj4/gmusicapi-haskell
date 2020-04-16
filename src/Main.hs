module Main where

import GMusicApi
import GMusicApi.Types

main :: IO ()
main = do
  tokenFile <- getDefaultTokenFile
  oauth2Token <- getAccessToken $ Just tokenFile
  manager <- getManager
  deviceManagementInfo <- getDeviceManagementInfo manager oauth2Token
  case deviceManagementInfo of
    Left e -> return ()
    Right (ListData devices) -> mapM_ (print . friendlyName) devices