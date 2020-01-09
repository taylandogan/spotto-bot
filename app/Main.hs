{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Control.Lens
import Data.Aeson (toJSON)
import Data.Aeson.Lens (key, nth)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Session as WSess
import System.Environment (getEnv)

baseSpotifyUrl :: String
baseSpotifyUrl = "https://api.spotify.com"

getAuthHeader :: IO String
getAuthHeader = do
    appId <- getEnv "SPOTIFY_APP_ID"
    appSecret <- getEnv "SPOTIFY_APP_SECRET"
    let concatted = B8.pack (appId ++ ":" ++ appSecret)
    return $ B8.unpack . encode $ concatted

main :: IO ()
main = do
    authHeader <- getAuthHeader
    sess <- WSess.newAPISession
    response <- WSess.get sess "http://google.com"
    print authHeader
