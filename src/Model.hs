{-# LANGUAGE DeriveGeneric #-}

module Model
    ( Token (..),
      Playlist,
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

data Playlist =
    Playlist { description :: !Text
                  , href :: !Text
                  , id :: !Text
                  , name :: !Text
                  , snapshot_id :: !Text
                   } deriving (Show, Generic)
instance FromJSON Playlist
instance ToJSON Playlist
