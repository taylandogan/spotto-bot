{-# LANGUAGE OverloadedStrings #-}

module Main where

import Model
import Playlist
import Track
import Utils

import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Aeson (toJSON, FromJSON, ToJSON, decode, eitherDecode)
import Data.Aeson.Lens (key, _String, _Array)
import Data.Aeson.Types (Value, parseJSON, parseEither, parseMaybe)
import Data.Either.Utils (maybeToEither)
import Data.Vector (Vector, concat, zip, (!?))
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as WS


username :: String
username = "taylandgn"

getToken :: WS.Session -> HTTPIO Token
getToken sess = do
    authHeader <- liftIO getAuthHeader
    let opts = addHeader "Authorization" authHeader W.defaults
    resp <- liftIO (WS.postWith opts sess tokenUrl ["grant_type" W.:= ("client_credentials" :: String)]) `catchE` requestHandler
    -- resp <- raiseResponse "Request to get token" resp
    return $ MkToken (resp ^. W.responseBody . key "access_token" . _String)

parsePlaylist :: Value -> HTTPIO Playlist
parsePlaylist val = case parseEither parseJSON val of
                            Left err -> throwError err
                            Right playlist -> return playlist

parseTrack :: Value -> HTTPIO Track
parseTrack val = case parseEither parseJSON val of
                        Left err -> throwError err
                        Right tr -> return . track $ tr

getPlaylists :: Int -> Int -> String -> WS.Session -> Token -> HTTPIO (Vector Playlist)
getPlaylists offset limit username sess token = do
    let offsetTxt = T.pack . show $ offset
    let limitTxt = T.pack . show $ limit
    let playlistUrl = baseSpotifyUrl ++ "/v1/users/" ++ username  ++ "/playlists"
    let opts = addQueryParam "offset" offsetTxt . addQueryParam "limit" limitTxt . addHeader "Authorization" (getBearerStr token) $ W.defaults
    resp <- liftIO (WS.getWith opts sess playlistUrl) `catchE` requestHandler
    mapM parsePlaylist (resp ^. W.responseBody . key "items" . _Array)

-- TODO: Do this until you get a empty list (but like who has 250 playlist?)
collectPlaylists :: String -> WS.Session -> Token -> HTTPIO (Vector Playlist)
collectPlaylists username sess token = Data.Vector.concat <$> traverse (\offset-> getPlaylists offset 50 username sess token) [0..5]

getTracksOfPlaylist :: Int -> Int -> Playlist -> WS.Session -> Token -> HTTPIO (Vector Track)
getTracksOfPlaylist offset limit playlist sess token = do
    let offsetTxt = T.pack . show $ offset
    let limitTxt = T.pack . show $ limit
    let url = tracksUrl playlist
    let opts = addQueryParam "offset" offsetTxt . addQueryParam "limit" limitTxt . addHeader "Authorization" (getBearerStr token) $ W.defaults
    resp <- liftIO (WS.getWith opts sess url) `catchE` requestHandler
    mapM parseTrack (resp ^. W.responseBody . key "items" . _Array)

-- TODO: Do this until you get a empty list (but like who has 1000 songs in a playlist?)
collectTracks :: WS.Session -> Token -> Playlist -> HTTPIO (Vector Track)
collectTracks sess token pl = Data.Vector.concat <$> traverse (\offset -> getTracksOfPlaylist offset 100 pl sess token) [0..10]

-- TODO: Remove duplicate items in vectors for both playlists and tracks
-- TODO: Concurrently create .csv files for each playlist
main :: IO ()
main = join . fmap (either print print) $ runExceptT $ do
    sess <- liftIO WS.newAPISession
    token <- getToken sess
    playlists <- collectPlaylists username sess token
    liftIO $ print "Retrieved playlists"
    tracks <- mapM (collectTracks sess token) playlists
    return $ Data.Vector.zip playlists tracks

