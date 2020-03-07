module Model
    ( Token (..),
      HTTPIO,
      InfoMsg
    ) where

import Control.Monad.Except
import Data.Text

type InfoMsg = String
type HTTPIO = ExceptT String IO
newtype Token = MkToken Text deriving (Show)
