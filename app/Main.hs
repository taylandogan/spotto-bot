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
import Network.HTTP.Types.Header as HH
import Network.Wreq as W
import Network.Wreq.Session as WS
import Network.Wreq.Types as WT
import System.Environment (getEnv)

newtype Token = MkToken T.Text deriving (Show)

baseSpotifyUrl :: String
baseSpotifyUrl = "https://api.spotify.com"

tokenUrl :: String
tokenUrl = "https://accounts.spotify.com/api/token"

getAuthHeader :: IO T.Text
getAuthHeader = do
    appId <- getEnv "SPOTIFY_APP_ID"
    appSecret <- getEnv "SPOTIFY_APP_SECRET"
    let concatted = B8.pack (appId ++ ":" ++ appSecret)
    return . T.pack $ "Basic " ++ (B8.unpack . encode $ concatted)

requestHandler :: HttpException -> IO (Either String (Response LBS.ByteString))
requestHandler (HttpExceptionRequest _ exceptionContent) = return $ Left $ show exceptionContent
requestHandler (InvalidUrlException url reason) = return $ Left $ "Invalid url: " ++ url ++ " Reason: " ++ reason

getToken :: WS.Session -> IO (Either String Token)
getToken sess = do
    authHeader <- getAuthHeader
    let opts = addHeader "Authorization" authHeader W.defaults
    resp <- (Right <$> WS.postWith opts sess tokenUrl ["grant_type" := ("client_credentials" :: String)]) `E.catch` requestHandler
    return $ MkToken <$> maybeToEither "Could not retrieve token" (resp ^? _Right . W.responseBody . key "access_token" . _String)

getBearerStr :: Token -> T.Text
getBearerStr (MkToken token) = T.pack $ "Bearer " ++ T.unpack token

addHeader :: HH.HeaderName -> T.Text -> WT.Options -> WT.Options
addHeader name value opts = opts & W.header name .~ [encodeUtf8 value]

addQueryParam :: T.Text -> T.Text -> WT.Options -> WT.Options
addQueryParam name value opts = opts & W.param name .~ [value]

main :: IO ()
main = do
    sess <- newAPISession
    eiToken <- getToken sess
    print eiToken
