module RikardCorp.Helpers.Json (
module RikardCorp.Helpers.Json,
eitherDecode', decode'
) where

import           Data.Aeson
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

decodeE :: FromJSON a => T.Text -> Either String a
decodeE = eitherDecode' . BL.fromStrict . TE.encodeUtf8

decodeT :: FromJSON a => T.Text -> Maybe a
decodeT = decode' . BL.fromStrict . TE.encodeUtf8

decodeE' :: FromJSON a => B.ByteString -> Either String a
decodeE' = eitherDecode' . BL.fromStrict

decodeT' :: FromJSON a => B.ByteString -> Maybe a
decodeT' = decode' . BL.fromStrict
