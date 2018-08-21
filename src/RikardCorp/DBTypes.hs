{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module RikardCorp.DBTypes (
module RikardCorp.DBTypes,
module RikardCorp.DBEnum
) where

import           Data.ByteString
import           Data.Text
import           Data.Time.Clock
import           Database.Persist.TH
import           RikardCorp.DBEnum

share [mkPersist sqlSettings, mkMigrate "migrateRikardCorpDB"] [persistLowerCase|
Tag
  name Text
  UniqueTagName name
  deriving Show
News
  date UTCTime
  header Text
  body Text
  htmlBody Text Maybe
  origin Text Maybe
  tags [TagId]
  deriving Show
NewsTagIndex
  tag TagId
  news NewsId
  UniqueTagIndex tag news
  deriving Show
NewsMonitor
  url Text
  postStatus PostStatus
  nid NewsId Maybe
  date UTCTime
  UniqueNewsMonitor url
Article
  date UTCTime
  header Text
  body Text
  htmlBody Text Maybe
  tags [TagId]
  deriving Show
ArticleTagIndex
  tag TagId
  article ArticleId
  UniqueArticleTagIndex tag article
  deriving Show
GeoAo
  name Text
  abbr Text
  json ByteString sqltype=MEDIUMBLOB
  deriving Show
GeoMo
  name Text
  nameAo Text
  json ByteString sqltype=MEDIUMBLOB
  deriving Show
AddrStreet sql=addrstreet
  guid Text sql=AOGUID sqltype=VARCHAR(50)
  code Text sql=CODE sqltype=VARCHAR(50)
  formalName Text sql=FORMALNAME sqltype=VARCHAR(50)
  offName Text sql=OFFNAME sqltype=VARCHAR(50)
  shortName Text sql=SHORTNAME sqltype=VARCHAR(50)
  selection Text Maybe
  centroid Text Maybe
  rank Int Maybe
  deriving Show
AddrHouse sql=addrhouse
  guid Text sql=AOGUID sqltype=VARCHAR(50)
  buildNum Text sql=BUILDNUM sqltype=VARCHAR(50)
  houseGuid Text sql=HOUSEGUID sqltype=VARCHAR(50)
  houseNum Text sql=HOUSENUM sqltype=VARCHAR(50)
  ifnsFl Text sql=IFNSFL sqltype=VARCHAR(50)
  ifnsUl Text sql=IFNSUL sqltype=VARCHAR(50)
  okato Text sql=OKATO sqltype=VARCHAR(50)
  oktmo Text sql=OKTMO sqltype=VARCHAR(50)
  postalCode Text sql=POSTALCODE sqltype=VARCHAR(50)
  strNum Text sql=STRUCNUM sqltype=VARCHAR(50)
  strStatus Text sql=STRSTATUS sqltype=VARCHAR(50)
  additions Text Maybe
  centroid Text Maybe
  detailsUrl Text Maybe
  detailsText Text Maybe
  deriving Show
GeoObject
  dataId Text sqltype=VARCHAR(50)
  ver Int
  size Int
  desc Text
  json ByteString sqltype=LONGBLOB
  date UTCTime
ForeignGeoObject
  dataId Text sqltype=VARCHAR(50)
  ver Int
  size Int
  desc Text
  json ByteString sqltype=LONGBLOB
  date UTCTime
GisRubric
  gisId Text
  name Text
  alias Text
GisFirm
  rubId GisRubricId
  gisId Text
  lon Text sqltype=VARCHAR(50)
  lat Text sqltype=VARCHAR(50)
  name Text
  hash Text
  address Text
  additions Text Maybe
RentPremAdv
  arch Bool
  nfNum Text Maybe sqltype=VARCHAR(50)
  nfLotNum Text Maybe sqltype=VARCHAR(50)
  ao Okrug Maybe
  mo Rayon Maybe
  sguid Text sqltype=VARCHAR(50)
  hguid Text sqltype=VARCHAR(50)
  addrTxt Text sqltype=VARCHAR(150)
  area Double
  areaGr Ploshad
  floor [Text] sqltype=VARCHAR(150)
  tenancyYr Int
  tenancyMn Int
  tenancyDy Int
  startPriceMetMn Double
  startPriceMetYr Double
  startMonthPrice Double
  startYearPrice Double
  mission [Naznachenie]
  expireDate UTCTime
  auctionDate UTCTime
  placeProps Text Maybe
  businessProps Text Maybe
  advProps Text Maybe
  tradeResult Text Maybe
  files Text Maybe
|]
