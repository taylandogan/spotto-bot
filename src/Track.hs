{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Track
    ( Track,
      TrackItem (track),
      trackHeader,
    ) where

import Artist

import Data.Aeson (FromJSON, ToJSON)
import Data.Csv (FromRecord, ToRecord, FromNamedRecord, ToNamedRecord, Header)
import Data.Text (Text)
import Data.Vector (fromList)
import GHC.Generics

trackHeader :: Header
trackHeader = fromList ["artists", "name", "id", "popularity"]

data Track =
    Track { artists :: [Artist]
            , name :: !Text
            , id :: !Text
            , popularity :: Int
           } deriving (Show, Generic)
instance FromJSON Track
instance ToJSON Track
instance FromRecord Track
instance ToRecord Track
instance FromNamedRecord Track
instance ToNamedRecord Track

newtype TrackItem = TrackItem { track :: Track} deriving (Show, Generic)
instance FromJSON TrackItem
instance ToJSON TrackItem