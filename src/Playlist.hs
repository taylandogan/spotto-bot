{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Playlist
    ( Playlist (id),
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import GHC.Generics

data Playlist =
    Playlist { id :: !Text
              , name :: !Text
              , href :: !Text
              , description :: !Text
              , snapshot_id :: !Text
              } deriving (Show, Generic)

instance FromJSON Playlist
instance ToJSON Playlist