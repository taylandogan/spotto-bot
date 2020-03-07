{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Playlist
    ( Playlist (id),
    ) where

import Data.Text
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data Playlist =
    Playlist { description :: !Text
                  , href :: !Text
                  , id :: !Text
                  , name :: !Text
                  , snapshot_id :: !Text
                   } deriving (Show, Generic)
instance FromJSON Playlist
instance ToJSON Playlist
