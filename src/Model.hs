{-# LANGUAGE DeriveGeneric #-}

module Model
    ( Token (..),
      Track,
      Tracks,
      PlaylistItem,
      HTTPIO,
      InfoMsg
    ) where

import Control.Monad.Except
import Data.Text
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

type InfoMsg = String
type HTTPIO = ExceptT String IO
newtype Token = MkToken Text deriving (Show)

data Track = Track { track_name :: ! Text } deriving (Show, Generic)
instance FromJSON Track
instance ToJSON Track

data Tracks = Tracks { tracks_href :: !Text
                    , total :: Int
                    } deriving (Show, Generic)
instance FromJSON Tracks
instance ToJSON Tracks

data PlaylistItem =
    PlaylistItem { description :: !Text
                  , href :: !Text
                  , id :: !Text
                  , name :: !Text
                  , snapshot_id :: !Text
                  , tracks :: Tracks
                    } deriving (Show, Generic)
instance FromJSON PlaylistItem
instance ToJSON PlaylistItem
