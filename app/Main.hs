{-# LANGUAGE OverloadedStrings #-}

module Main where

import Const
import Model
import Playlist
import Track
import Utils

import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Concurrent (threadDelay)
import Data.Aeson.Lens (key, _Array)
import Data.Aeson.Types (Value, parseJSON, parseEither)
import Data.Csv (encodeByNameWith, encIncludeHeader, defaultEncodeOptions)
import Data.List (sum)
import Data.Vector (Vector, concat, zip, toList, length)
import Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as WS
import System.Directory


parsePlaylist :: Value -> HTTPIO Playlist
parsePlaylist val = case parseEither parseJSON val of
                            Left err -> throwError err
                            Right playlist -> return playlist

parseTrack :: Value -> HTTPIO Track
parseTrack val = case parseEither parseJSON val of
                        Left err -> throwError err
                        Right tr -> return . track $ tr

getPlaylists :: Int -> Int -> WS.Session -> Token -> HTTPIO (Vector Playlist)
getPlaylists offset limit sess token = do
    let opts = optsLimitOffset limit offset token
    liftIO $ threadDelay halfSecondInMicroseconds -- due to rate-limiting
    resp <- liftIO (WS.getWith opts sess playlistUrl) `catchE` requestHandler
    mapM parsePlaylist (resp ^. W.responseBody . key "items" . _Array)

getTracksOfPlaylist :: Int -> Int -> Playlist -> WS.Session -> Token -> HTTPIO (Vector Track)
getTracksOfPlaylist offset limit playlist sess token = do
    let url = tracksUrl playlist
    let opts = optsLimitOffset limit offset token
    liftIO $ threadDelay halfSecondInMicroseconds -- due to rate-limiting
    resp <- liftIO (WS.getWith opts sess url) `catchE` requestHandler
    mapM parseTrack (resp ^. W.responseBody . key "items" . _Array)

-- TODO: Do this until you get a empty list (but like who has 250 playlist?)
collectPlaylists :: WS.Session -> Token -> HTTPIO (Vector Playlist)
collectPlaylists sess token = Data.Vector.concat <$> traverse (\offset-> getPlaylists offset 50 sess token) ((50*) <$> [0..5])

-- TODO: Do this until you get a empty list (but like who has 1000 songs in a playlist?)
collectTracks :: WS.Session -> Token -> Playlist -> HTTPIO (Vector Track)
collectTracks sess token pl = Data.Vector.concat <$> traverse (\offset -> getTracksOfPlaylist offset 100 pl sess token) ((100*) <$> [0..10])

dumpToFile :: (Playlist, Vector Track) -> IO ()
dumpToFile (pl, vTracks) = do
    let fileName = (T.unpack . name $ pl) ++ ".csv"
    let csvOptions = defaultEncodeOptions {encIncludeHeader = True}
    print $ "Exporting.. " ++ fileName
    currentDir <- getCurrentDirectory
    withCurrentDirectory currentDir $ B.writeFile fileName (LB.toStrict . encodeByNameWith csvOptions trackHeader $ toList vTracks)

main :: IO ()
main = join . fmap (either print print) $ runExceptT $ do
    sess <- liftIO WS.newAPISession
    token <- getToken sess
    liftIO $ print "Retrieving your playlists.."
    playlists <- collectPlaylists sess token
    liftIO $ print $ "Retrieved " ++ (show . Data.Vector.length $ playlists) ++ " playlists."
    liftIO $ print "Retrieving your tracks.."
    tracks <- mapM (collectTracks sess token) playlists
    liftIO $ print $ "Retrieved " ++ (show . sum $ Data.Vector.length <$> tracks) ++ " tracks."
    liftIO $ print "Writing into CSVs.."
    liftIO $ mapM dumpToFile (Data.Vector.zip playlists tracks)
    return "Done!"

