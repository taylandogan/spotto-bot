module Utils
    ( baseSpotifyUrl,
      accountsUrl,
      tokenUrl,
      getAuthHeader,
      getBearerStr,
      addHeader,
      addQueryParam,
      requestHandler
    ) where

import Model

import Control.Lens
import Control.Monad.Trans.Except
import Data.ByteString.Base64 (encode)
import Data.Text.Encoding (encodeUtf8)
import System.Environment (getEnv)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Network.HTTP.Types.Header as HH
import qualified Network.Wreq.Types as WT
import qualified Network.Wreq as W
import qualified Network.HTTP.Client as CL


baseSpotifyUrl :: String
baseSpotifyUrl = "https://api.spotify.com"

accountsUrl :: String
accountsUrl = "https://accounts.spotify.com"

tokenUrl :: String
tokenUrl = accountsUrl ++ "/api/token"

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

-- raiseResponse :: InfoMsg -> Response LBS.ByteString-> HTTPIO (Response LBS.ByteString)
-- raiseResponse info r = do
--     let status = r ^. W.responseStatus
--     let statusCode = status ^. W.statusCode
--     let statusMsg = show $ status ^. W.statusMessage
--     case statusCode of
--         200 -> return r
--         _ -> throwE $ info ++ " : FAILED with status: " ++ show statusCode ++ " " ++ statusMsg
