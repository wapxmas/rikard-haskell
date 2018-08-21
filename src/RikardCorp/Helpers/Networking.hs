{-# LANGUAGE OverloadedStrings #-}

module RikardCorp.Helpers.Networking (
    module RikardCorp.Helpers.Networking
  , NHT.QueryText
  ) where

import           Blaze.ByteString.Builder     (toByteString)
import           Control.Arrow                (second)
import           Control.Monad
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as BL
import           Data.Conduit
import qualified Data.Conduit.Binary          as CB
import qualified Data.List                    as DL
import           Data.Maybe
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8)
import           Network.Connection           (TLSSettings (..))
import qualified Network.HTTP.Client          as NHC
import qualified Network.HTTP.Client.TLS      as NHCT
import qualified Network.HTTP.Conduit         as NCOND
import           Network.HTTP.Types           (renderQueryText)
import qualified Network.HTTP.Types           as NHT
import qualified Network.URI                  as U
import qualified System.Directory             as SD
import           Yesod

type PreUrl = T.Text
type AbsoluteURL = String

type Cookies = Maybe B.ByteString

getUrlQueryParams :: T.Text -> Maybe NHT.QueryText
getUrlQueryParams url =
  let
    uri = U.parseURI . T.unpack $ url
  in
    if isNothing uri
      then Nothing
      else getUrlQueryParams' . U.uriQuery . fromJust $ uri
  where
    getUrlQueryParams' :: String -> Maybe NHT.QueryText
    getUrlQueryParams' = Just . NHT.parseQueryText . B8.pack

renderYesodRoute :: RenderRoute route => PreUrl -> Route route -> T.Text
renderYesodRoute txt route =
  let
    (pieces, params) = renderRoute route
  in
    txt `T.append` T.concat ("/" : DL.intersperse "/" pieces) `T.append` decodeUtf8 (toByteString $ renderQueryText True (map (second Just) params))

renderUrl :: PreUrl -> [T.Text] -> [(T.Text, T.Text)] -> T.Text
renderUrl txt pieces params =
    txt `T.append` T.concat ("/" : DL.intersperse "/" pieces) `T.append` decodeUtf8 (toByteString $ renderQueryText True (map (second Just) params))

mkRequest :: String -> NHC.Request
mkRequest = NHC.parseRequest_

setUserAgentHeader :: String -> NHC.Request -> NHC.Request
setUserAgentHeader userAgent request =
  request { NHC.requestHeaders =
              replaceRequestHeader
                NHT.hUserAgent
                (B8.pack userAgent)
                (NHC.requestHeaders request)
          }

setCookiesHeader :: Cookies -> NHC.Request -> NHC.Request
setCookiesHeader Nothing request = request
setCookiesHeader (Just data') request =
  request { NHC.requestHeaders =
              replaceRequestHeader
                NHT.hCookie
                data'
                (NHC.requestHeaders request)
          }

getContentsWithCookiesByURL :: Cookies -> String -> String -> IO (AbsoluteURL, B.ByteString, Cookies)
getContentsWithCookiesByURL cookies userAgent addr = do
  let request =
          setCookiesHeader cookies
        . setUserAgentHeader userAgent
        $ mkRequest addr
  manager <- NHC.newManager (NHCT.mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  res <- NHC.httpLbs request manager
  return (getAbsoluteURL addr, BL.toStrict . NHC.responseBody $ res, getCookies (NHC.responseHeaders res))
  where
    getCookies :: NHT.ResponseHeaders -> Maybe B.ByteString
    getCookies [] = Nothing
    getCookies ((header, data'):rhs)
      | header == ("Set-Cookie" :: NHT.HeaderName) = Just data'
      | otherwise = getCookies rhs

downloadFileByURL :: String -> String -> Bool -> IO ()
downloadFileByURL url filePath deleteFile = do
  fileExists <- SD.doesFileExist filePath
  when (deleteFile && fileExists) $
    SD.removeFile filePath
  when (deleteFile || (not deleteFile && not fileExists)) $ do
    request <- NHC.parseRequest url
    manager <- NHC.newManager (NHCT.mkManagerSettings (TLSSettingsSimple True False False) Nothing)
    runResourceT $ do
           response <- NCOND.http request manager
           NHC.responseBody response $$+- CB.sinkFile filePath

getContentsByURL :: String -> String -> IO (AbsoluteURL, B.ByteString)
getContentsByURL userAgent addr = do
  let
    request :: NHC.Request
    request = NHC.parseRequest_ addr

    requestHeaders' :: NHT.RequestHeaders
    requestHeaders' = replaceRequestHeader NHT.hUserAgent (B8.pack userAgent) $ NHC.requestHeaders request

    request' :: NHC.Request
    request' = request { NHC.requestHeaders = requestHeaders' }

  manager <- NHC.newManager (NHCT.mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  res <- NHC.httpLbs request' manager
  return (getAbsoluteURL addr, BL.toStrict . NHC.responseBody $ res)

replaceRequestHeader :: NHT.HeaderName -> B.ByteString -> NHT.RequestHeaders -> NHT.RequestHeaders
replaceRequestHeader hName hValue headers = (hName, hValue) : [(_name, _value) | (_name, _value) <- headers, _name /= hName]

getAbsoluteURL :: String -> AbsoluteURL
getAbsoluteURL s =
  let
    (_type, _rest) = DL.break (==':') s
    (_host, _rest2) = DL.break (\c -> c == '/' || c == '?') $ DL.drop 3 _rest
  in
    _type ++ "://" ++ _host
