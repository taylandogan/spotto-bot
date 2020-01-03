{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Control.Lens
import Data.Aeson (toJSON)
import Data.Aeson.Lens (key, nth)
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Session as WSess

main :: IO ()
main = do
    sess <- WSess.newAPISession
    response <- WSess.get sess "http://google.com"
    print response
