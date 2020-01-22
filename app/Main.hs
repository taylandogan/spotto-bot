{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Control.Exception as E
import Control.Lens
import Control.Lens.Combinators (_Right)
import Data.Aeson (toJSON)
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Base64 (encode)
import Data.Either.Utils (maybeToEither, fromRight)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.Maybe (fromMaybe)
import Network.HTTP.Client as CL
import Network.Wreq as W
import Network.Wreq.Session as WS
import System.Environment (getEnv)

newtype Token = MkToken T.Text deriving(Show)

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

requestHandler :: HttpException -> IO (Either String (Response LBS.ByteString))
requestHandler (HttpExceptionRequest _ exceptionContent) = return $ Left $ show exceptionContent
requestHandler (InvalidUrlException url reason) = return $ Left $ "Invalid url: " ++ url ++ " Reason: " ++ reason

getToken :: WS.Session -> IO (Either String Token)
getToken sess = do
    authHeader <- getAuthHeader
    let opts = W.defaults & W.header "Authorization" .~ [B8.pack authHeader]
    resp <- (Right <$> WS.postWith opts sess tokenUrl ["grant_type" := ("client_credentials" :: String)]) `E.catch` requestHandler
    return $ MkToken <$> maybeToEither "Could not retrieve token" (resp ^? _Right . W.responseBody . key "access_token" . _String)

main :: IO ()
main = do
    sess <- newAPISession
    eiToken <- getToken sess
    print eiToken
