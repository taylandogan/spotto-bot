{-# LANGUAGE OverloadedStrings #-}

module Main where

import Model
import Utils

import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Aeson (toJSON, FromJSON, ToJSON, decode, eitherDecode)
import Data.Aeson.Lens (key, _String, _Array)
import Data.Aeson.Types (Value, parseJSON, parseEither, parseMaybe)
import Data.Either.Utils (maybeToEither)
import Data.Vector (Vector, (!?))
import qualified Data.ByteString.Lazy as B
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

parsePlaylist :: Value -> HTTPIO PlaylistItem
parsePlaylist val = case parseEither parseJSON val of
                            Left err -> throwError err
                            Right pl -> return pl

getMyPlaylists :: WS.Session -> Token -> HTTPIO (Vector PlaylistItem)
getMyPlaylists sess token = do
    let playlistUrl = baseSpotifyUrl ++ "/v1/users/taylandgn/playlists"
    let opts = addQueryParam "offset" "0" . addQueryParam "limit" "32" . addHeader "Authorization" (getBearerStr token) $ W.defaults
    resp <- liftIO (WS.getWith opts sess playlistUrl) `catchE` requestHandler
    mapM parsePlaylist (resp ^. W.responseBody . key "items" . _Array)

main :: IO ()
main = do
    sess <- WS.newAPISession
    eiPlaylists <- runExceptT $ getMyPlaylists sess =<< getToken sess
    print eiPlaylists
