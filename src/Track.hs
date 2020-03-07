{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Track
    ( Track,
      TrackItem (track),
    ) where

import Artist

import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import GHC.Generics

data Track =
    Track { id :: !Text
            , name :: !Text
            , popularity :: Int
            , artists :: [Artist]
           } deriving (Show, Generic)
instance FromJSON Track
instance ToJSON Track


newtype TrackItem = TrackItem { track :: Track} deriving (Show, Generic)
instance FromJSON TrackItem
instance ToJSON TrackItem