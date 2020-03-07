{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Artist
    ( Artist,
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import GHC.Generics

data Artist =
    Artist { id :: !Text
             , name :: !Text
            } deriving (Show, Generic)
instance FromJSON Artist
instance ToJSON Artist
