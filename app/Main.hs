{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Control.Lens
import Data.Aeson (toJSON)
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Base64 (encode)
import Data.Text (unpack)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Network.Wreq (defaults, header, responseBody, FormParam((:=)))
import Network.Wreq.Session (Session, postWith, newAPISession)
import System.Environment (getEnv)

baseSpotifyUrl :: String
baseSpotifyUrl = "https://api.spotify.com"

tokenUrl :: String
tokenUrl = "https://accounts.spotify.com/api/token"

getAuthHeader :: IO String
getAuthHeader = do
    appId <- getEnv "SPOTIFY_APP_ID"
    appSecret <- getEnv "SPOTIFY_APP_SECRET"
    let concatted = B8.pack (appId ++ ":" ++ appSecret)
    return $ "Basic " ++ (B8.unpack . encode $ concatted)

getToken :: Session -> IO (Maybe String)
getToken sess = do
    authHeader <- getAuthHeader
    let opts = defaults & header "Authorization" .~ [B8.pack authHeader]
    resp <- postWith opts sess tokenUrl ["grant_type" := ("client_credentials" :: String)]
    return $ unpack <$> resp ^? responseBody . key "access_token" . _String

main :: IO ()
main = do
    sess <- newAPISession
    mbAccessToken <- getToken sess
    print mbAccessToken
