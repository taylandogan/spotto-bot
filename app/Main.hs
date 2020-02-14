{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Aeson (toJSON, FromJSON, ToJSON, decode)
import Data.Aeson.Lens (key, _String, _Array)
import Data.Aeson.Types (Value)
import Data.ByteString.Base64 (encode)
import Data.Either.Utils (maybeToEither, fromRight)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import GHC.Generics
import Network.HTTP.Client as CL
import Network.HTTP.Types.Header as HH
import Network.Wreq as W
import Network.Wreq.Session as WS
import Network.Wreq.Types as WT
import System.Environment (getEnv)

newtype Token = MkToken T.Text deriving (Show)

baseSpotifyUrl :: String
baseSpotifyUrl = "https://api.spotify.com"

accountsUrl :: String
accountsUrl = "https://accounts.spotify.com"

tokenUrl :: String
tokenUrl = accountsUrl ++ "/api/token"

getAuthHeader :: IO T.Text
getAuthHeader = do
    appId <- getEnv "SPOTIFY_APP_ID"
    appSecret <- getEnv "SPOTIFY_APP_SECRET"
    let concatted = B8.pack (appId ++ ":" ++ appSecret)
    return . T.pack $ "Basic " ++ (B8.unpack . encode $ concatted)

getBearerStr :: Token -> T.Text
getBearerStr (MkToken token) = T.pack $ "Bearer " ++ T.unpack token

addHeader :: HH.HeaderName -> T.Text -> WT.Options -> WT.Options
addHeader name value opts = opts & W.header name .~ [encodeUtf8 value]

addQueryParam :: T.Text -> T.Text -> WT.Options -> WT.Options
addQueryParam name value opts = opts & W.param name .~ [value]

requestHandler :: HttpException -> HTTPIO (Response LBS.ByteString)
requestHandler (HttpExceptionRequest _ exceptionContent) = throwE . show $ exceptionContent
requestHandler (InvalidUrlException url _) = throwE $ "Invalid url: " ++ url

-- raiseResponse :: InfoMsg -> Response LBS.ByteString-> HTTPIO (Response LBS.ByteString)
-- raiseResponse info r = do
--     let status = r ^. W.responseStatus
--     let statusCode = status ^. W.statusCode
--     let statusMsg = show $ status ^. W.statusMessage
--     case statusCode of
--         200 -> return r
--         _ -> throwE $ info ++ " : FAILED with status: " ++ show statusCode ++ " " ++ statusMsg

getToken :: WS.Session -> HTTPIO Token
getToken sess = do
    authHeader <- liftIO $ getAuthHeader
    let opts = addHeader "Authorization" authHeader W.defaults
    resp <- liftIO (WS.postWith opts sess tokenUrl ["grant_type" := ("client_credentials" :: String)]) `catchE` requestHandler
    -- resp <- raiseResponse "Request to get token" resp
    return $ MkToken (resp ^. W.responseBody . key "access_token" . _String)

getMyPlaylists :: WS.Session -> Token -> HTTPIO (Vector Value)
getMyPlaylists sess token = do
    let playlistUrl = baseSpotifyUrl ++ "/v1/users/taylandgn/playlists"
    let opts = addQueryParam "offset" "0" . addQueryParam "limit" "1" . addHeader "Authorization" (getBearerStr token) $ W.defaults
    resp <- liftIO (WS.getWith opts sess playlistUrl) `catchE` requestHandler
    return $ resp ^. W.responseBody . key "items" . _Array

main :: IO ()
main = do
    sess <- newAPISession
    eiToken <- runExceptT $ getToken sess
    case eiToken of
        Left err -> print err
        Right token -> print =<< (runExceptT $ getMyPlaylists sess token)
