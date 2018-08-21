{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module RikardCorp.Types(
module RikardCorp.Types,
module RikardCorp.DBTypes
) where

import qualified BasicPrelude           as BP
import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Attoparsec.Text   as P
import           Data.Default
import qualified Data.HashTable.IO      as HT
import           Data.List              as DL
import qualified Data.Map.Strict        as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set               as S
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Database.Persist.MySQL
import qualified Geo.Computations       as GEO
import qualified Geo.Types              as GEO ()
import           RikardCorp.DBTypes
import           RikardCorp.Static
import           Text.Read
import           Yesod                  (mkYesodData, parseRoutes)
import qualified Yesod                  as Y
import           Yesod.EmbeddedStatic

type TagsMap = HT.CuckooHashTable T.Text TagId
type TagsIdsMap = HT.CuckooHashTable Int Tag

data RikardCorp = RikardCorp {
  getStatic        :: EmbeddedStatic,
  connectionsPool  :: Maybe ConnectionPool,
  getMasterTagsMap :: Maybe TagsIdsMap
}

instance Default RikardCorp where
  def = RikardCorp {
    getStatic = rikardStatic,
    connectionsPool = Nothing,
    getMasterTagsMap = Nothing
  }

mkYesodData "RikardCorp" [parseRoutes|
/ IndexR GET
!/#USection/#UOkrug/#UPloshad/#UNaznach/#UPage MainR GET
/a ArticlesR GET
/a/#UPage ArticlesIndexR GET
!/a/#ArticleId ArticleTextR GET
/n NewsR GET
/n/#UPage NewsIndexR GET
!/n/#NewsId NewsTextR GET
/n/tags/#TagId/#UPage NewsTagsIndexR GET
/c ContactsR GET
/w LetterR GET POST
/adda AdditionsR GET
/fraud FraudR GET
/favicon.ico FaviconR GET
/robots.txt RobotsR GET
/static StaticR EmbeddedStatic getStatic
|]

routeSnippet :: Maybe (Y.Route RikardCorp) -> T.Text
routeSnippet (Just (MainR (USection (Just sect)) _ _ _ _)) = snippet (sectionData sect)
routeSnippet _ = "Коммерческая недвижимость, жилая недвижимость, земельные участки в аренду и собственность от города Москвы"

type PageNum = Int
type PageLimit = Int
type ElementsOnPage = Int

data SectionData = SectionData {
    title         :: T.Text
  , shortTitle    :: T.Text
  , xsTitle       :: T.Text
  , mapTitle      :: T.Text
  , xsMapTitle    :: T.Text
  , pageTitle     :: T.Text
  , menuTitle     :: T.Text
  , advShortTitle :: T.Text
  , snippet       :: T.Text
  , keywords      :: T.Text
} deriving Eq

data SectionType = SectRent | SectPPA | SectDirectRent | SectProperty | SectLandRent deriving (Eq, Show, Enum, Bounded, Read)

data Section = Section SectionType SectionData deriving Eq

newtype USection = USection (Maybe SectionType) deriving (Show,Eq,Read)
newtype UOkrug = UOkrug (Maybe Okrug) deriving (Show,Eq,Read)
newtype UPloshad = UPloshad (Maybe Ploshad) deriving (Show,Eq,Read)
newtype UNaznach = UNaznach (Maybe Naznachenie) deriving (Show,Eq,Read)
newtype UPage = UPage (Maybe Int) deriving (Show,Eq,Read)

type GEOPoint      = GEO.Point
type GEOLine       = [GEOPoint]
type GEOPolygon    = [GEOPoint]
data GEOCoordinate =
    LINESTRING      GEOLine
  | MULTILINESTRING [GEOLine]
  | POLYGON         GEOPolygon
  | MULTIPOLYGON    [GEOPolygon]
  | POINT           GEOPoint
  deriving Show

data GeoJson = GeoJson
  { geoJsonType     :: T.Text
  , geoJsonCRS      :: GeoJsonCRS
  , geoJsonFeatures :: [GeoJsonFeature]
  } deriving Show

data GeoJsonCRS = GeoJsonCRS
  { geoJsonCRSType           :: T.Text
  , geoJsonCRSTypeProperties :: M.Map T.Text T.Text
  } deriving Show

data GeoJsonFeature = GeoJsonFeature
  { geoJsonFeatureType       :: T.Text
  , geoJsonFeatureProperties :: M.Map T.Text T.Text
  , geoJsonFeatureGeometry   :: GeoJsonGeometry
  } deriving Show

data GeoJsonGeometry
  = GeoJsonGeometryPolygon { geoJsonGeometryPolygonCoordinates :: GeoJsonPolygon }
  | GeoJsonGeometryMultiPolygon { geoJsonGeometryMultiPolygonCoordinates :: GeoJsonMultiPolygon }
  deriving Show

newtype GeoJsonMultiPolygon = GeoJsonMultiPolygon
  { geoJsonMultiPolygonObjects :: [GeoJsonPolygon]
  } deriving Show

newtype GeoJsonPolygon = GeoJsonPolygon
  { geoJsonPolygonObjects :: [GeoJsonLinearRing]
  } deriving Show

newtype GeoJsonLinearRing = GeoJsonLinearRing
  { geoJsonLinearRingPoints :: [GeoJsonPoint]
  } deriving Show

newtype GeoJsonPoint = GeoJsonPoint
  { geoJsonPointCoords :: [Double]
  } deriving Show

data GisResponse = GisResponse
  { gisResponseCode    :: Int
  , gisResponseTotal   :: Int
  , gisResponseObjects :: [GisObject]
  } deriving Show

data GisObject = GisObject
  { gisObjectType       :: T.Text
  , gisObjectSelection  :: T.Text
  , gisObjectCentroid   :: T.Text
  , gisObjectAttributes :: GisObjectAttributes
  } deriving Show

data GisObjectAttributes = GisObjectAttributes
  { gisObjectAttributesRank         :: Maybe Int
  , gisObjectAttributesPurpose      :: Maybe T.Text
  , gisObjectAttributesFirmCount    :: Maybe Int
  , gisObjectAttributesBuildingName :: Maybe T.Text
  , gisObjectAttributesPostalIndex  :: Maybe T.Text
  , gisObjectAttributesElevation    :: Maybe Int
  , gisObjectAttributesSynonym      :: Maybe T.Text
  , gisObjectAttributesInfo         :: Maybe T.Text
  } deriving Show

data DomMosResult = DomMosResult
  { domMosResultValue :: T.Text
  , domMosResultUrl   :: T.Text
  } deriving Show

newtype AddrHouseAddtionsData = AddrHouseAddtionsData
  { gisAttributes :: Maybe GisObjectAttributes
  } deriving Show

data HouseStewardship
  = HouseStewardship
    { houseStewardshipData :: Maybe (M.Map T.Text (S.Set T.Text))
    , houseStewardshipInfo :: Maybe T.Text
    }
    deriving Show

newtype HouseMainInfo
  = HouseMainInfo
    { houseMainInfoData :: Maybe (M.Map T.Text T.Text)
    }
    deriving Show

newtype HouseExtraInfo
  = HouseExtraInfo
    { houseExtraInfoData :: Maybe (M.Map T.Text (M.Map T.Text T.Text))
    }
    deriving Show

data HouseInfo
  = HouseInfo
    { houseInfoStewardship :: Maybe HouseStewardship
    , houseInfoMain        :: Maybe HouseMainInfo
    , houseInfoExtra       :: Maybe HouseExtraInfo
    }
    deriving Show

data MVDistrict
  = MVDistrict
    { mvDistrictName :: T.Text
    , mvDistrictSubject :: T.Text
    , mvDistrictLat :: T.Text
    , mvDistrictLng :: T.Text
    , mvDistrictStreet :: T.Text
    , mvDistrictHouse :: T.Text
    }

instance ToJSON MVDistrict where
  toJSON (MVDistrict n s lat lng str house) =
    object
      [ "name" .= n
      , "subject" .= s
      , "lat" .= lat
      , "lng" .= lng
      , "street" .= str
      , "house" .= house
      ]

instance Show MVDistrict where
  show (MVDistrict n s lat lng str house) =
       T.unpack n
    <> " "
    <> T.unpack s
    <> " "
    <> T.unpack lat
    <> " "
    <> T.unpack lng
    <> " "
    <> T.unpack str
    <> " "
    <> T.unpack house

instance FromJSON HouseInfo where
  parseJSON (Object v) = do
    stewardship' <- v .:? "info_stewardship"
    main'  <- v .:? "info_main"
    extra' <- v .:? "info_extra"
    return $ HouseInfo stewardship' main' extra'
  parseJSON invalid = typeMismatch "HouseInfo" invalid

instance ToJSON HouseInfo where
  toJSON (HouseInfo stewardship' main' extra') =
    object
      [ "info_stewardship" .= stewardship'
      , "info_main" .= main'
      , "info_extra" .= extra'
      ]

instance FromJSON HouseExtraInfo where
  parseJSON (Object v) = do
    data' <- v .:? "data"
    return $ HouseExtraInfo data'
  parseJSON invalid = typeMismatch "HouseExtraInfo" invalid

instance ToJSON HouseExtraInfo where
  toJSON (HouseExtraInfo data') =
    object
      [ "data" .= data'
      ]

instance FromJSON HouseMainInfo where
  parseJSON (Object v) = do
    data' <- v .:? "data"
    return $ HouseMainInfo data'
  parseJSON invalid = typeMismatch "HouseMainInfo" invalid

instance ToJSON HouseMainInfo where
  toJSON (HouseMainInfo data') =
    object
      [ "data" .= data'
      ]

instance FromJSON HouseStewardship where
  parseJSON (Object v) = do
    data' <- v .:? "data"
    info <- v .:? "info"
    return $ HouseStewardship data' info
  parseJSON invalid = typeMismatch "HouseStewardship" invalid

instance ToJSON HouseStewardship where
  toJSON (HouseStewardship data' info) =
    object
      [ "data" .= data'
      , "info" .= info
      ]

instance FromJSON DomMosResult where
  parseJSON (Object v) = do
    value <- v .: "value"
    url   <- v .: "url"
    return $ DomMosResult value url
  parseJSON invalid = typeMismatch "DomMosResult" invalid

instance FromJSON GisResponse where
  parseJSON (Object v) = do
    code <- read <$> v .: "response_code"
    total <- read <$> v .: "total"
    objects <- v .: "result"
    return $ GisResponse code total objects
  parseJSON invalid = typeMismatch "GisResponse" invalid

instance ToJSON GisResponse where
  toJSON (GisResponse code total objects) =
    object
      [ "code" .= code
      , "total" .= total
      , "objects" .= objects
      ]

instance FromJSON GisObject where
  parseJSON (Object v) = do
    tp <- v .: "type"
    selection <- v .: "selection"
    centroid <- v .: "centroid"
    attrs <- v .: "attributes"
    return $ GisObject tp selection centroid attrs
  parseJSON invalid = typeMismatch "GisObject" invalid

instance ToJSON GisObject where
  toJSON (GisObject tp selection centroid attrs) =
    object
      [ "type" .= tp
      , "selection" .= selection
      , "centroid" .= centroid
      , "attributes" .= attrs
      ]

instance FromJSON AddrHouseAddtionsData where
  parseJSON (Object v) =
    AddrHouseAddtionsData
      <$> v .:? "gisAttributes"
  parseJSON invalid = typeMismatch "AddrHouseAddtionsData" invalid

instance ToJSON AddrHouseAddtionsData where
  toJSON (AddrHouseAddtionsData attrs) =
    object
      [ "gisAttributes" .= attrs
      ]

instance FromJSON GisObjectAttributes where
  parseJSON (Object v) =
    GisObjectAttributes
      <$> v .:? "rank"
      <*> v .:? "purpose"
      <*> v .:? "firmcount"
      <*> v .:? "buildingname"
      <*> v .:? "index"
      <*> (v .:? "elevation"
            <|> (readMaybe . fromMaybe ""
            <$> (v .:? "elevation")))
      <*> v .:? "synonym"
      <*> v .:? "info"
  parseJSON invalid = typeMismatch "GisObjectAttributes" invalid

instance ToJSON GisObjectAttributes where
  toJSON (GisObjectAttributes rank purpose firmcount
    buildingname index elevation synonym info) =
    object
      [ "rank" .= rank
      , "purpose" .= purpose
      , "firmcount" .= firmcount
      , "buildingname" .= buildingname
      , "index" .= index
      , "elevation" .= elevation
      , "synonym" .= synonym
      , "info" .= info
      ]

instance FromJSON GeoJson where
  parseJSON (Object v) = do
    tp <- v .: "type"
    crs <- v .: "crs"
    fs <- v .: "features"
    return $ GeoJson tp crs fs
  parseJSON invalid = typeMismatch "GeoJson" invalid

instance ToJSON GeoJson where
  toJSON (GeoJson tp crs fs) = object ["type" .= tp, "crs" .= crs, "features" .= fs]

instance FromJSON GeoJsonFeature where
  parseJSON (Object v) = do
    tp <- v .: "type"
    props <- v .: "properties"
    geom <- v .: "geometry"
    return $ GeoJsonFeature tp props geom
  parseJSON invalid = typeMismatch "GeoJsonFeature" invalid

instance ToJSON GeoJsonFeature where
  toJSON (GeoJsonFeature tp props geom) = object ["type" .= tp, "properties" .= props, "geometry" .= geom]

instance FromJSON GeoJsonGeometry where
  parseJSON (Object v) = do
    tp <- v .: "type"
    case tp of
      "Polygon" -> do
        coords <- v .: "coordinates"
        return $ GeoJsonGeometryPolygon coords
      "MultiPolygon" -> do
        coords <- v .: "coordinates"
        return $ GeoJsonGeometryMultiPolygon coords
      _ -> fail $ "Undefined geometry type: " <> tp
  parseJSON invalid = typeMismatch "GeoJsonGeometry" invalid

instance ToJSON GeoJsonGeometry where
  toJSON (GeoJsonGeometryPolygon coords) =
    object
      [ "type" .= ("Polygon" :: T.Text)
      , "coordinates" .= coords
      ]
  toJSON (GeoJsonGeometryMultiPolygon coords) =
    object
      [ "type" .= ("MultiPolygon" :: T.Text)
      , "coordinates" .= coords
      ]

instance FromJSON GeoJsonMultiPolygon where
  parseJSON obj = do
    pgs <- parseJSON obj :: Parser [GeoJsonPolygon]
    return $ GeoJsonMultiPolygon pgs

instance ToJSON GeoJsonMultiPolygon where
  toJSON (GeoJsonMultiPolygon ps) = toJSON ps

instance FromJSON GeoJsonPolygon where
  parseJSON obj = do
    lrs <- parseJSON obj :: Parser [GeoJsonLinearRing]
    return $ GeoJsonPolygon lrs

instance ToJSON GeoJsonPolygon where
  toJSON (GeoJsonPolygon lrs) = toJSON lrs

instance FromJSON GeoJsonLinearRing where
  parseJSON obj = do
    pts <- parseJSON obj :: Parser [GeoJsonPoint]
    return $ GeoJsonLinearRing pts

instance ToJSON GeoJsonLinearRing where
  toJSON (GeoJsonLinearRing pts) = toJSON pts

instance FromJSON GeoJsonPoint where
  parseJSON (Array ps)
    | V.length ps == 2 = do
          lon <- parseJSON $ ps V.! 0
          lat <- parseJSON $ ps V.! 1
          return $ GeoJsonPoint [lon, lat]
    | otherwise = fail "Not GeoJson point coordinates"
  parseJSON invalid = typeMismatch "GeoJsonPoint" invalid

instance ToJSON GeoJsonPoint where
  toJSON (GeoJsonPoint coords) = toJSON coords

instance FromJSON GeoJsonCRS where
  parseJSON (Object v) = do
    tp <- v .: "type"
    props <- v .: "properties"
    return $ GeoJsonCRS tp props
  parseJSON invalid = typeMismatch "GeoJsonCRS" invalid

instance ToJSON GeoJsonCRS where
  toJSON (GeoJsonCRS tp props) =
    object
      [ "type" .= tp
      , "properties" .= props
      ]

instance Y.PathPiece USection where
  fromPathPiece t = if t == nothingPieceDef then Just (USection Nothing) else
    case Y.fromPathPiece t of (Just i) -> Just (USection . Just $ toEnum i); _ -> Nothing
  toPathPiece (USection v) = case v of Nothing -> nothingPieceDef; Just u -> BP.tshow $ fromEnum u

instance Y.PathPiece UOkrug where
  fromPathPiece t = if t == nothingPieceDef then Just (UOkrug Nothing) else
    case Y.fromPathPiece t of (Just i) -> Just (UOkrug . Just $ toEnum i); _ -> Nothing
  toPathPiece (UOkrug v) = case v of Nothing -> nothingPieceDef; Just u -> BP.tshow $ fromEnum u

instance Y.PathPiece UPloshad where
  fromPathPiece t = if t == nothingPieceDef then Just (UPloshad Nothing) else
    case Y.fromPathPiece t of (Just i) -> Just (UPloshad . Just $ toEnum i); _ -> Nothing
  toPathPiece (UPloshad v) = case v of Nothing -> nothingPieceDef; Just u -> BP.tshow $ fromEnum u

instance Y.PathPiece UNaznach where
  fromPathPiece t = if t == nothingPieceDef then Just (UNaznach Nothing) else
    case Y.fromPathPiece t of (Just i) -> Just (UNaznach . Just $ toEnum i); _ -> Nothing
  toPathPiece (UNaznach v) = case v of Nothing -> nothingPieceDef; Just u -> BP.tshow $ fromEnum u

instance Y.PathPiece UPage where
  fromPathPiece t = if t == nothingPieceDef then Just (UPage Nothing) else
    case T.uncons t of Just ('p', t') -> Just (UPage $ Y.fromPathPiece t'); _ -> Nothing
  toPathPiece (UPage v) = case v of Nothing -> nothingPieceDef; Just i -> 'p' `T.cons` BP.tshow i

instance Default SectionData where
  def = SectionData {
      title = T.empty
    , shortTitle = T.empty
    , xsTitle = T.empty
    , mapTitle = T.empty
    , xsMapTitle = T.empty
    , pageTitle = T.empty
    , menuTitle = T.empty
    , advShortTitle = T.empty
    , snippet = T.empty
    , keywords = T.empty
}

setPageOpts :: PageLimit -> PageNum -> [SelectOpt rec]
setPageOpts pl pn = [OffsetBy (pl * (pn - 1)), LimitTo (pl + 1)]

getPageNum :: UPage -> Int
getPageNum (UPage Nothing) = 1
getPageNum (UPage (Just pnum)) = pnum

isNewsRoute :: Maybe (Y.Route RikardCorp) -> Bool
isNewsRoute (Just NewsR) = True
isNewsRoute (Just (NewsIndexR _)) = True
isNewsRoute (Just (NewsTextR _)) = True
isNewsRoute _ = False

sectionTypeToRoute :: SectionType -> Y.Route RikardCorp
sectionTypeToRoute st = MainR (usect st) uokrugNothing uploshadNothing unaznachNothing upageNothing

routeToSectionType :: Maybe (Y.Route RikardCorp) -> Maybe SectionType
routeToSectionType (Just (MainR (USection (Just st)) _ _ _ _)) = Just st
routeToSectionType _ = Nothing

usectNothing :: USection
usectNothing = USection Nothing

usect :: SectionType -> USection
usect = USection . Just

uokrugNothing :: UOkrug
uokrugNothing = UOkrug Nothing

uokrug :: Okrug -> UOkrug
uokrug = UOkrug . Just

uploshadNothing :: UPloshad
uploshadNothing = UPloshad Nothing

uploshad :: Ploshad -> UPloshad
uploshad = UPloshad . Just

unaznachNothing :: UNaznach
unaznachNothing = UNaznach Nothing

unaznach :: Naznachenie -> UNaznach
unaznach = UNaznach . Just

upageNothing :: UPage
upageNothing = UPage Nothing

upage :: Int -> UPage
upage = UPage . Just

nothingPieceDef :: T.Text
nothingPieceDef = "z"

sectionTypeToInt :: SectionType -> Int
sectionTypeToInt = fromEnum

intToSectionType :: Int -> SectionType
intToSectionType = toEnum

sectionData :: SectionType -> SectionData
sectionData SectRent = def {
    title         = "Права аренды от города Москвы"
  , shortTitle    = "Права аренды"
  , xsTitle       = "Права аренды"
  , mapTitle      = "Права аренды на карте"
  , xsMapTitle    = "Права аренды<br>на карте"
  , pageTitle     = "Права аренды от города Москвы"
  , menuTitle     = "Аренда от города Москвы"
  , advShortTitle = "Права аренды"
  , snippet       = "Коммерческая недвижимость офис, магазин, торговая площадь, общепит, автосервис, медицина и др. в аренду от города Москвы"
  , keywords      = "права аренды, аренда с правом выкупа, договор права аренды, права аренды москва, переуступка прав аренды, аренда правом выкупа "
                  <>"москва, аукцион право аренды, аукцион на право заключения договора аренды, уступка права аренды, договор переуступки права аренды, "
                  <>"право аренды 49 лет, аренда 49 лет правом выкупа, права аренды города, права аренды города москве, москомимущество аренда, помещение "
                  <>"от города, аренда помещения у города, аренда помещений от москомимущества, аренда нежилых помещений в москве от города, помещения от "
                  <>"москомимущества, аукцион продаже права аренды, проведение аукциона право аренды, проведение аукциона на право заключения договора аренды, "
                  <>"аукцион право аренды муниципального имущества, аукцион нежилых помещений, аукцион на заключение договора аренды, право на заключение "
                  <>"договора аренды, торги аренда, аукционы помещений москва, аукцион на аренду помещения, аукцион нежилых помещений москва, аукцион на аренду "
                  <>"нежилых помещений, аренда помещений у города москвы аукцион, аукционы по аренде помещений москва, аукционы продаже нежилых помещений москве, "
                  <>"помещения выставленные аукцион, аукцион право аренды нежилого помещения, проведение аукциона аренда помещения, мосимущество аукцион нежилых "
                  <>"помещений, муниципальные помещения аукцион, аукционы на право заключения договора аренды"
}
sectionData SectPPA = def {
    title         = "Продажа ППА"
  , shortTitle    = "Продажа ППА, прав аренды"
  , xsTitle       = "Продажа ППА"
  , mapTitle      = "Продажа ППА на карте"
  , xsMapTitle    = "Продажа ППА<br>на карте"
  , pageTitle     = "Продажа ППА"
  , menuTitle     = "Продажа ППА"
  , advShortTitle = "Продажа ППА"
  , snippet       = "Продажа ППА офис, магазин, торговая площадь, общепит, автосервис, медицина и др. прав аренды от города Москвы"
  , keywords      = "продажа ппа, продажа ппа москве, ппа продажа прав аренды, ппа продажа права аренды в москве, продажа ппа от дигм, "
                  <>"продажа ппа от города, продажа ппа от города москвы, продажа через ппа, ппа продажа права аренды от частных лиц, "
                  <>"ппа продажа частных лиц, продажа помещений ппа, ппа в москве, купить ппа, продажа права аренды, купить ппа в москве, "
                  <>"переуступка аренды, продажа прав аренды нежилых помещений в москве"
}
sectionData SectDirectRent = def {
    title         = "Прямая аренда"
  , shortTitle    = "Прамая аренда от собственника"
  , xsTitle       = "Прямая аренда"
  , mapTitle      = "Прямая аренда на карте"
  , xsMapTitle    = "Прямая аренда<br>на карте"
  , pageTitle     = "Помещения в аренду от собственника"
  , menuTitle     = "Прямая аренда"
  , advShortTitle = "Прямая аренда"
  , snippet       = "Прямая аренда офис, магазин, торговая площадь, общепит, автосервис, медицина и др. от собственника"
  , keywords      = "прямая аренда, москва аренда прямая, прямая аренда от собственника, аренда офиса прямая, прямая аренда помещения, прямая "
                  <>"аренда офиса от собственника, аренда от собственника, аренда от собственника москва, аренда помещений от собственника, аренда "
                  <>"офиса от собственника, аренда помещений свободного назначения от собственника, аренда помещения в москве от собственника, аренда "
                  <>"офисов в москве от собственника, помещение под аренду собственника, аренда автосервиса от собственника, аренда торговых помещений "
                  <>"от собственника, аренда торговых помещений в москве от собственника, аренда магазина от собственника, автомойка в аренду от собственника, "
                  <>"аренда собственников кафе, аренда нежилых помещений от собственника, аренда столовых от собственника, аренда недвижимости от собственника, "
                  <>"аренда склада от собственника, аренда автомойки от собственника в москве, аренда нежилых помещений в москве от собственника, аренда кафе в "
                  <>"москве от собственника, аренда автосервиса в москве от собственника, помещение от собственника, офис от собственника, недвижимость от "
                  <>"собственников, снять от собственника"
}
sectionData SectProperty = def {
    title         = "Собственность"
  , shortTitle    = "Собственность"
  , xsTitle       = "Собственность"
  , mapTitle      = "Собственность на карте"
  , xsMapTitle    = "Собственность<br>на карте"
  , pageTitle     = "Помещения в собственность"
  , menuTitle     = "Собственность"
  , advShortTitle = "Собственность"
  , snippet       = "Собственность офис, магазин, торговая площадь, общепит, автосервис, медицина и др. от города Москвы"
  , keywords      = "купить нежилое помещение, купить нежилое помещение в москве, продажа нежилых помещений, купить помещение "
                  <>"в москве, продажа помещений, нежилые помещения в москве, продажа нежилого, продам нежилое помещение, продам помещение, "
                  <>"коммерческое помещение купить, купить коммерческую недвижимость, продам нежилое, покупка нежилого помещения, продажа нежилой недвижимости"
}
sectionData SectLandRent = def {
    title         = "Земельные участки"
  , shortTitle    = "Земельные участки"
  , xsTitle       = "Земельные участки"
  , mapTitle      = "Земельные участки на карте"
  , xsMapTitle    = "Земельные участки<br>на карте"
  , pageTitle     = "Земельные участки"
  , menuTitle     = "Земельные участки"
  , advShortTitle = "Земельный участок"
  , snippet       = "Земельные участки офис, магазин, торговая площадь, общепит, автосервис, медицина и др. в аренду от города Москвы"
  , keywords      = "аренда земельного участка, право аренды земельного участка, аренда земельного участка под, аренда земельного участка "
                  <>"в москве, аренда земельного участка под строительство, право на заключение договора аренды земельного участка, аренда "
                  <>"земельного участка на 49 лет, аукцион на право аренды земельного участка, аукцион по аренде земельных участков, аукцион "
                  <>"заключение договора аренды земельного участка, аукцион право заключения аренды земельного участка, аренда земельного участка "
                  <>"муниципальной собственности, аукционы продаже права аренды земельного участка, аукционы продаже аренде земельных участков, "
                  <>"аукцион на право заключения аренды земельного участка, организация аукциона на право аренды земельного участка, торги земельных участков, "
                  <>"земельные торги, торги на участки, аукцион по продаже участков, аукцион на землю, участок в аренду, земельные участки в городе, "
                  <>"муниципальные земельные участки, аукцион земельных участков, проведение аукциона земельный участок"
}

sectionsList :: [Section]
sectionsList = fmap (\st -> Section st $ sectionData st) $ [SectRent,SectProperty,SectLandRent] `union` [minBound .. ]

okrugsList :: [Okrug]
okrugsList = [minBound .. ]

rayonsList :: [Rayon]
rayonsList = [minBound .. ]

ploshadList :: [Ploshad]
ploshadList = [minBound .. ]

naznachXsList :: [[Naznachenie]]
naznachXsList =
  [
    [AVTOSALON, AVTOSERVIS, BYTOVYE_USLUGI, DOKTOR_RJADOM, MAGAZIN, MASHINOMESTO, MEDICINA, OBSHHEPIT, OFIS, SVOBODNOE_NAZNACHENIE, TORGOVAJA_PLOSHHAD]
  ]

naznachMdList :: [[Naznachenie]]
naznachMdList =
  [
    [AVTOSALON, AVTOSERVIS, AVTOSTOJANKA, AVTOMOJKA, APTEKA, BANK, BYTOVYE_USLUGI, GOSTINICA, DOKTOR_RJADOM, DOU, KAFE, MAGAZIN, MASHINOMESTO, MEDICINA, OBRAZOVATELNOE,
     OBSHHEPIT, OFIS, PROIZVODSTVO, RESTORAN, SVOBODNOE_NAZNACHENIE, SKLAD, STOLOVAJA, TORGOVAJA_PLOSHHAD, FOK]
  ]

naznachLgList :: [[Naznachenie]]
naznachLgList =
  [
    [AVTOSALON, AVTOSERVIS, AVTOSTOJANKA, AVTOMOJKA, APTEKA, BANK, BYTOVYE_USLUGI, GARAZH, GOSTINICA, DOKTOR_RJADOM, DOU, KAFE, KINOTEATR, MAGAZIN,
     MASHINOMESTO, MEDICINA, OBRAZOVATELNOE, OBSHHEPIT, OFIS, PROIZVODSTVO, REKLAMNAJA_PLOSHHAD, RESTORAN, SVOBODNOE_NAZNACHENIE, SKLAD, STOLOVAJA,
     TORGOVAJA_PLOSHHAD, FOK, NOCHNOJ_KLUB],
    [GOTOVYJ_BIZNES, KULTURNOE_NASLEDIE, RAZRESHENA_SUBARENDA, JEKSKLJUZIV, OTDELNO_STOJASHHEE_ZDANIE, NEZHILOE]
  ]

naznachToText :: Naznachenie -> T.Text
naznachToText n =
  case n of
    OFIS                      -> "офис"
    TORGOVAJA_PLOSHHAD        -> "торговая площадь"
    SKLAD                     -> "склад"
    OBSHHEPIT                 -> "общепит"
    SVOBODNOE_NAZNACHENIE     -> "свободное назначение"
    AVTOSTOJANKA              -> "автостоянка"
    PROIZVODSTVO              -> "производство"
    AVTOSERVIS                -> "автосервис"
    GOTOVYJ_BIZNES            -> "готовый бизнес"
    OTDELNO_STOJASHHEE_ZDANIE -> "отдельно стоящее здание"
    BYTOVYE_USLUGI            -> "бытовые услуги"
    MEDICINA                  -> "медицина"
    KULTURNOE_NASLEDIE        -> "культурное наследие"
    GOSTINICA                 -> "гостиница"
    ZEMELNYJ_UCHASTOK         -> "земельный участок"
    RAZRESHENA_SUBARENDA      -> "разрешена субаренда"
    PRODAZHA_PPA              -> "продажа ППА"
    GARAZH                    -> "гараж"
    KINOTEATR                 -> "кинотеатр"
    APTEKA                    -> "аптека"
    DOKTOR_RJADOM             -> "доктор рядом"
    KAFE                      -> "кафе"
    RESTORAN                  -> "ресторан"
    STOLOVAJA                 -> "столовая"
    MAGAZIN                   -> "магазин"
    AVTOSALON                 -> "автосалон"
    BANK                      -> "банк"
    FOK                       -> "ФОК"
    REKLAMNAJA_PLOSHHAD       -> "рекламная площадь"
    DOU                       -> "ДОУ"
    OBRAZOVATELNOE            -> "образовательное"
    NEZHILOE                  -> "нежилое"
    JEKSKLJUZIV               -> "эксклюзив"
    SOBSTVENNOST              -> "собственность"
    AVTOMOJKA                 -> "автомойка"
    NOCHNOJ_KLUB              -> "ночной клуб"
    KVARTIRA                  -> "квартира"
    PRJAMAJA_ARENDA           -> "прямая аренда"
    MASHINOMESTO              -> "машиноместо"

ploshadToRange :: Ploshad -> (Int, Int)
ploshadToRange p =
  case p of
    P1_50         -> (1,50)
    P50_100       -> (50,100)
    P100_200      -> (100,200)
    P200_500      -> (200,500)
    P500_1000     -> (500,1000)
    P1000_10000   -> (1000,10000)
    P10000_100000 -> (10000,100000)

ploshadToText :: Ploshad -> T.Text
ploshadToText p =
  let
    (from, to) = ploshadToRange p
  in BP.tshow from <> "-" <> BP.tshow to <> " кв.м."

okrugTextMap :: M.Map T.Text Okrug
okrugTextMap =
  let
    os :: [(T.Text, Okrug)]
    os = fmap (\o -> (okrugToText o, o)) [minBound ..]
  in
    M.fromList os

okrugToText :: Okrug -> T.Text
okrugToText o =
  case o of
    TSAO  -> "ЦАО"
    SAO   -> "САО"
    SVAO  -> "СВАО"
    VAO   -> "ВАО"
    YUVAO -> "ЮВАО"
    YUAO  -> "ЮАО"
    YUZAO -> "ЮЗАО"
    ZAO   -> "ЗАО"
    SZAO  -> "СЗАО"
    ZELAO -> "ЗелАО"
    TINAO -> "ТиНАО"

rayonTextMap :: M.Map T.Text Rayon
rayonTextMap =
  let
    os :: [(T.Text, Rayon)]
    os = fmap (\o -> (rayonToText o, o)) [minBound ..]
  in
    M.fromList os

rayonToText :: Rayon -> T.Text
rayonToText r =
  case r of
    KIEVSKIJ                  -> "Киевский"
    FILJOVSKIJ_PARK           -> "Филёвский Парк"
    NOVOFJODOROVSKOE          -> "Новофёдоровское"
    ROGOVSKOE                 -> "Роговское"
    MOSRENTGEN                -> "Мосрентген"
    VORONOVSKOE               -> "Вороновское"
    MIHAJLOVO_JARCEVSKOE      -> "Михайлово-Ярцевское"
    MARUSHKINSKOE             -> "Марушкинское"
    PERVOMAJSKOE              -> "Первомайское"
    MATUSHKINO                -> "Матушкино"
    VNUKOVO                   -> "Внуково"
    SAVJOLKI                  -> "Савёлки"
    VNUKOVSKOE                -> "Внуковское"
    SILINO                    -> "Силино"
    KOKOSHKINO                -> "Кокошкино"
    KRJUKOVO                  -> "Крюково"
    SHHUKINO                  -> "Щукино"
    KRASNOPAHORSKOE           -> "Краснопахорское"
    NAGATINSKIJ_ZATON         -> "Нагатинский Затон"
    STAROE_KRJUKOVO           -> "Старое Крюково"
    KLJONOVSKOE               -> "Клёновское"
    DMITROVSKIJ               -> "Дмитровский"
    FILIMONKOVSKOE            -> "Филимонковское"
    TROICK                    -> "Троицк"
    TJOPLYJ_STAN              -> "Тёплый Стан"
    SHHAPOVSKOE               -> "Щаповское"
    MOSKOVSKIJ                -> "Московский"
    DESJONOVSKOE              -> "Десёновское"
    HOVRINO                   -> "Ховрино"
    LOMONOSOVSKIJ             -> "Ломоносовский"
    MOZHAJSKIJ                -> "Можайский"
    NOVO_PEREDELKINO          -> "Ново-Переделкино"
    STROGINO                  -> "Строгино"
    MOLZHANINOVSKIJ           -> "Молжаниновский"
    MITINO                    -> "Митино"
    KURKINO                   -> "Куркино"
    KRYLATSKOE                -> "Крылатское"
    SOLNCEVO                  -> "Солнцево"
    SOSENSKOE                 -> "Сосенское"
    VOSKRESENSKOE             -> "Воскресенское"
    GOLOVINSKIJ               -> "Головинский"
    JUZHNOE_TUSHINO           -> "Южное Тушино"
    SEVERNOE_TUSHINO          -> "Северное Тушино"
    CHERJOMUSHKI              -> "Черёмушки"
    POKROVSKOE_STRESHNEVO     -> "Покровское-Стрешнево"
    HOROSHJOVO_MNJOVNIKI      -> "Хорошёво-Мнёвники"
    OCHAKOVO_MATVEEVSKOE      -> "Очаково-Матвеевское"
    TROPARJOVO_NIKULINO       -> "Тропарёво-Никулино"
    LEVOBEREZHNYJ             -> "Левобережный"
    FILI_DAVYDKOVO            -> "Фили-Давыдково"
    OBRUCHEVSKIJ              -> "Обручевский"
    RJAZANOVSKOE              -> "Рязановское"
    RAMENKI                   -> "Раменки"
    VOJKOVSKIJ                -> "Войковский"
    SOKOL                     -> "Сокол"
    ZAPADNOE_DEGUNINO         -> "Западное Дегунино"
    PROSPEKT_VERNADSKOGO      -> "Проспект Вернадского"
    JUZHNOE_BUTOVO            -> "Южное Бутово"
    JASENEVO                  -> "Ясенево"
    DOROGOMILOVO              -> "Дорогомилово"
    KONKOVO                   -> "Коньково"
    HOROSHJOVSKIJ             -> "Хорошёвский"
    BEGOVOJ                   -> "Беговой"
    KOPTEVO                   -> "Коптево"
    SHHERBINKA                -> "Щербинка"
    AJEROPORT                 -> "Аэропорт"
    PRESNENSKIJ               -> "Пресненский"
    SEVERNYJ                  -> "Северный"
    BESKUDNIKOVSKIJ           -> "Бескудниковский"
    GAGARINSKIJ               -> "Гагаринский"
    TIMIRJAZEVSKIJ            -> "Тимирязевский"
    SEVERNOE_BUTOVO           -> "Северное Бутово"
    LIANOZOVO                 -> "Лианозово"
    HAMOVNIKI                 -> "Хамовники"
    VOSTOCHNOE_DEGUNINO       -> "Восточное Дегунино"
    SAVJOLOVSKIJ              -> "Савёловский"
    AKADEMICHESKIJ            -> "Академический"
    ZJUZINO                   -> "Зюзино"
    ALTUFEVSKIJ               -> "Алтуфьевский"
    MARFINO                   -> "Марфино"
    CHERTANOVO_CENTRALNOE     -> "Чертаново Центральное"
    OTRADNOE                  -> "Отрадное"
    ARBAT                     -> "Арбат"
    CHERTANOVO_JUZHNOE        -> "Чертаново Южное"
    BUTYRSKIJ                 -> "Бутырский"
    TVERSKOJ                  -> "Тверской"
    CHERTANOVO_SEVERNOE       -> "Чертаново Северное"
    JAKIMANKA                 -> "Якиманка"
    KOTLOVKA                  -> "Котловка"
    OSTANKINSKIJ              -> "Останкинский"
    DONSKOJ                   -> "Донской"
    BIBIREVO                  -> "Бибирево"
    BIRJULJOVO_ZAPADNOE       -> "Бирюлёво Западное"
    MARINA_ROSHHA             -> "Марьина Роща"
    NAGORNYJ                  -> "Нагорный"
    SVIBLOVO                  -> "Свиблово"
    DANILOVSKIJ               -> "Даниловский"
    MESHHANSKIJ               -> "Мещанский"
    JUZHNOE_MEDVEDKOVO        -> "Южное Медведково"
    ZAMOSKVORECHE             -> "Замоскворечье"
    SEVERNOE_MEDVEDKOVO       -> "Северное Медведково"
    NAGATINO_SADOVNIKI        -> "Нагатино-Садовники"
    MOSKVORECHE_SABUROVO      -> "Москворечье-Сабурово"
    CARICYNO                  -> "Царицыно"
    BASMANNYJ                 -> "Басманный"
    KRASNOSELSKIJ             -> "Красносельский"
    ROSTOKINO                 -> "Ростокино"
    TAGANSKIJ                 -> "Таганский"
    ALEKSEEVSKIJ              -> "Алексеевский"
    SOKOLNIKI                 -> "Сокольники"
    BIRJULJOVO_VOSTOCHNOE     -> "Бирюлёво Восточное"
    BABUSHKINSKIJ             -> "Бабушкинский"
    JUZHNOPORTOVYJ            -> "Южнопортовый"
    JAROSLAVSKIJ              -> "Ярославский"
    PECHATNIKI                -> "Печатники"
    BOGORODSKOE               -> "Богородское"
    METROGORODOK              -> "Метрогородок"
    LEFORTOVO                 -> "Лефортово"
    OREHOVO_BORISOVO_SEVERNOE -> "Орехово-Борисово Северное"
    LOSINOOSTROVSKIJ          -> "Лосиноостровский"
    NIZHEGORODSKIJ            -> "Нижегородский"
    PEROVO                    -> "Перово"
    OREHOVO_BORISOVO_JUZHNOE  -> "Орехово-Борисово Южное"
    MARINO                    -> "Марьино"
    VESHNJAKI                 -> "Вешняки"
    PREOBRAZHENSKOE           -> "Преображенское"
    SOKOLINAJA_GORA           -> "Соколиная Гора"
    LJUBLINO                  -> "Люблино"
    TEKSTILSHHIKI             -> "Текстильщики"
    BRATEEVO                  -> "Братеево"
    ZJABLIKOVO                -> "Зябликово"
    RJAZANSKIJ                -> "Рязанский"
    IZMAJLOVO                 -> "Измайлово"
    NOVOGIREEVO               -> "Новогиреево"
    KUZMINKI                  -> "Кузьминки"
    GOLJANOVO                 -> "Гольяново"
    SEVERNOE_IZMAJLOVO        -> "Северное Измайлово"
    KAPOTNJA                  -> "Капотня"
    VYHINO_ZHULEBINO          -> "Выхино-Жулебино"
    VOSTOCHNYJ                -> "Восточный"
    VOSTOCHNOE_IZMAJLOVO      -> "Восточное Измайлово"
    IVANOVSKOE                -> "Ивановское"
    KOSINO_UHTOMSKIJ          -> "Косино-Ухтомский"
    NOVOKOSINO                -> "Новокосино"
    NEKRASOVKA                -> "Некрасовка"
    KUNCEVO                   -> "Кунцево"

geoInvertLinearRingPolygon :: GeoJsonPolygon -> GeoJsonPolygon
geoInvertLinearRingPolygon (GeoJsonPolygon lrs) =
  GeoJsonPolygon (fmap geoInvertLinearRing lrs)

geoInvertLinearRing :: GeoJsonLinearRing -> GeoJsonLinearRing
geoInvertLinearRing (GeoJsonLinearRing pts) =
  GeoJsonLinearRing (fmap geoInvertPoint pts)

geoInvertPoint :: GeoJsonPoint -> GeoJsonPoint
geoInvertPoint (GeoJsonPoint coords) = GeoJsonPoint (reverse coords)

geoConvertMultiToPolygon :: GeoJson -> GeoJson
geoConvertMultiToPolygon (GeoJson tp crs features) =
  GeoJson tp crs (geoConvertMultiToPolygon' features)
  where
    geoConvertMultiToPolygon' :: [GeoJsonFeature] -> [GeoJsonFeature]
    geoConvertMultiToPolygon' [] = []
    geoConvertMultiToPolygon' (f@(GeoJsonFeature _ _ (
      GeoJsonGeometryPolygon _)):fs) =
          f
        : geoConvertMultiToPolygon' fs
    geoConvertMultiToPolygon' (GeoJsonFeature tp' prop' m@(
      GeoJsonGeometryMultiPolygon _):fs) =
          GeoJsonFeature tp' prop' (geoMultiToPolygon m)
        : geoConvertMultiToPolygon' fs

geoMultiToPolygon :: GeoJsonGeometry -> GeoJsonGeometry
geoMultiToPolygon (GeoJsonGeometryMultiPolygon (GeoJsonMultiPolygon parr)) =
  GeoJsonGeometryPolygon (GeoJsonPolygon (concatMap geoJsonPolygonObjects parr))
geoMultiToPolygon p = p

parseGEOGisString :: T.Text -> Maybe GEOCoordinate
parseGEOGisString gisGeoText =
  case P.parseOnly geoParser gisGeoText of
    Right coords -> Just coords
    _ -> Nothing

geoParser :: P.Parser GEOCoordinate
geoParser =
    P.choice
      [ geoPointParser
      , geoLinestringParser
      , geoMultiLinestringParser
      , geoPolygonParser
      , geoMultiPolygonParser]
  <* P.endOfInput

geoMultiPolygonParser :: P.Parser GEOCoordinate
geoMultiPolygonParser = do
  void $ P.asciiCI "MULTIPOLYGON"
  void $ P.char '('
  mpgn <- MULTIPOLYGON <$> geoPolygone `P.sepBy1` P.char ','
  void $ P.char ')'
  return mpgn

geoPolygonParser :: P.Parser GEOCoordinate
geoPolygonParser = do
  void $ P.asciiCI "POLYGON"
  POLYGON <$> geoPolygone

geoMultiLinestringParser :: P.Parser GEOCoordinate
geoMultiLinestringParser = do
  void $ P.asciiCI "MULTILINESTRING"
  void $ P.char '('
  lns <- MULTILINESTRING <$> geoLine `P.sepBy1` P.char ','
  void $ P.char ')'
  return lns

geoLinestringParser :: P.Parser GEOCoordinate
geoLinestringParser = do
  void $ P.asciiCI "LINESTRING"
  LINESTRING <$> geoLine

geoPointParser :: P.Parser GEOCoordinate
geoPointParser = do
  void $ P.asciiCI "POINT"
  void $ P.char '('
  pt <- POINT <$> geoPoint
  void $ P.char ')'
  return pt

geoPoint :: P.Parser GEOPoint
geoPoint = do
  lon <- P.double
  P.skipSpace
  lat <- P.double
  return $ GEO.pt lat lon Nothing Nothing

geoLine :: P.Parser GEOLine
geoLine = do
  void $ P.char '('
  pts <- geoPoint `P.sepBy1` P.char ','
  void $ P.char ')'
  return pts

geoPolygone :: P.Parser GEOPolygon
geoPolygone = do
  void $ P.char '('
  pgn <- geoLine
  void $ P.char ')'
  return pgn

splitPointsByMeters :: Double -> Bool -> GEO.Point -> GEO.Point -> GEOLine
splitPointsByMeters mets addEndPoint pStart pEnd =
  let pts = (pStart : DL.unfoldr splitDistance pStart)
  in
  if addEndPoint
    then pts <> [pEnd]
    else pts
  where
    splitDistance :: GEO.Point -> Maybe (GEO.Point, GEO.Point)
    splitDistance p =
      let dist :: Double
          dist = GEO.distance p pEnd

          md :: Double
          md = mets + 5
      in
      if dist > md
        then
          let np = GEO.interpolate p pEnd (mets / dist)
          in Just (np, np)
        else Nothing

splitGEOCdsByMeters :: Double -> GEOCoordinate -> GEOCoordinate
splitGEOCdsByMeters mets (MULTILINESTRING ls)
  = MULTILINESTRING
  $ fmap (concat . splitGEOLine mets) ls
splitGEOCdsByMeters mets (LINESTRING ln)
  = LINESTRING
  $ concat (splitGEOLine mets ln)
splitGEOCdsByMeters _ a = a

splitGEOLine :: Double -> GEOLine -> [GEOLine]
splitGEOLine _ [] = []
splitGEOLine mets (p1:p2:ps)
  = splitPointsByMeters mets False p1 p2
  : splitGEOLine mets (p2:ps)
splitGEOLine mets (p:ps)
  = [p]
  : splitGEOLine mets ps
