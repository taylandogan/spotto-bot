{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( getToken,
      getAuthHeader,
      getBearerStr,
      addHeader,
      addQueryParam,
      requestHandler,
      optsLimitOffset
    ) where

import Const
import Model
import Playlist

import Control.Lens
import Control.Monad.Except (liftIO)
import Control.Monad.Trans.Except
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Base64 (encode)
import Data.Text.Encoding (encodeUtf8)
import System.Environment (getEnv)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Network.HTTP.Types.Header as HH
import qualified Network.Wreq.Types as WT
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as WS
import qualified Network.HTTP.Client as CL



getToken :: WS.Session -> HTTPIO Token
getToken sess = do
    authHeader <- liftIO getAuthHeader
    let opts = addHeader "Authorization" authHeader W.defaults
    resp <- liftIO (WS.postWith opts sess tokenUrl ["grant_type" W.:= ("client_credentials" :: String)]) `catchE` requestHandler
    return $ MkToken (resp ^. W.responseBody . key "access_token" . _String)

getAuthHeader :: IO T.Text
getAuthHeader = do
    appId <- getEnv "SPOTIFY_APP_ID"
    appSecret <- getEnv "SPOTIFY_APP_SECRET"
    let concatted = B8.pack (appId ++ ":" ++ appSecret)
    return . T.pack $ "Basic " ++ (B8.unpack . encode $ concatted)

getBearerStr :: Token -> T.Text
getBearerStr (MkToken token) = T.pack $ "Bearer " ++ T.unpack token

addHeader :: HH.HeaderName -> T.Text -> WT.Options -> WT.Options
addHeader name value opts = opts & W.header name .~ [encodeUtf8 value]

addQueryParam :: T.Text -> T.Text -> WT.Options -> WT.Options
addQueryParam name value opts = opts & W.param name .~ [value]

requestHandler :: CL.HttpException -> HTTPIO (W.Response LBS.ByteString)
requestHandler (CL.HttpExceptionRequest _ exceptionContent) = throwE . show $ exceptionContent
requestHandler (CL.InvalidUrlException url _) = throwE $ "Invalid url: " ++ url

optsLimitOffset :: Int -> Int -> Token -> W.Options
optsLimitOffset limit offset token =
  let offsetTxt = intToText offset in
  let limitTxt = intToText limit in
  addQueryParam "offset" offsetTxt . addQueryParam "limit" limitTxt . addHeader "Authorization" (getBearerStr token) $ W.defaults

intToText :: Int -> T.Text
intToText = T.pack . show