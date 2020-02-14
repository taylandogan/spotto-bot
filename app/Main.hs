{-# LANGUAGE OverloadedStrings #-}

module Main where

import Model
import Utils

import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Aeson (toJSON, FromJSON, ToJSON, decode)
import Data.Aeson.Lens (key, _String, _Array)
import Data.Aeson.Types (Value)
import Data.Either.Utils (maybeToEither, fromRight)
import Data.Vector (Vector)
import qualified Data.Text as T
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as WS


getToken :: WS.Session -> HTTPIO Token
getToken sess = do
    authHeader <- liftIO getAuthHeader
    let opts = addHeader "Authorization" authHeader W.defaults
    resp <- liftIO (WS.postWith opts sess tokenUrl ["grant_type" W.:= ("client_credentials" :: String)]) `catchE` requestHandler
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
    sess <- WS.newAPISession
    eiToken <- runExceptT $ getToken sess
    case eiToken of
        Left err -> print err
        Right token -> print =<< (runExceptT $ getMyPlaylists sess token)
