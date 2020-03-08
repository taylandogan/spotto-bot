module Const
    ( baseSpotifyUrl,
      accountsUrl,
      tracksUrl,
      tokenUrl,
      playlistUrl,
      username,
      halfSecondInMicroseconds
    ) where

import Playlist
import Data.Text (unpack)

username :: String
username = "taylandgn"

halfSecondInMicroseconds :: Int
halfSecondInMicroseconds = 500000

baseSpotifyUrl :: String
baseSpotifyUrl = "https://api.spotify.com"

accountsUrl :: String
accountsUrl = "https://accounts.spotify.com"

tokenUrl :: String
tokenUrl = accountsUrl ++ "/api/token"

playlistUrl :: String
playlistUrl = baseSpotifyUrl ++ "/v1/users/" ++ username  ++ "/playlists"

tracksUrl :: Playlist -> String
tracksUrl pl = baseSpotifyUrl ++ "/v1/playlists/" ++ (unpack . Playlist.id $ pl) ++ "/tracks"
