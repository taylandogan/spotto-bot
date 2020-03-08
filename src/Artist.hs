{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Artist
    ( Artist,
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as B8
import Data.Csv (FromNamedRecord, ToNamedRecord, FromField, ToField, parseField, toField)
import Data.List as L
import Data.List.Split as TS
import Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import GHC.Generics

data Artist =
    Artist { id :: Text
             , name :: !Text
            } deriving (Show, Generic)
instance FromJSON Artist
instance ToJSON Artist
instance FromNamedRecord Artist where
instance ToNamedRecord Artist

instance FromField [Artist] where
    parseField aList = pure $ (\a -> Artist{id = "", name = T.pack a}) <$> aNames
                        where aNames = TS.splitOn ", " $ B8.unpack aList

instance ToField [Artist] where
    toField aList = B.pack $ L.intercalate ", " $ T.unpack . name <$> aList
