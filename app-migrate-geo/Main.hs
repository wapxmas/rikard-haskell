{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Arrow
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.UTF8          as SU8
import qualified Data.Char                     as C
import           Data.Function
import qualified Data.HashTable.IO             as HT
import qualified Data.List                     as DL
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Ord                      as O
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.IO                  as TIO
import           Database.Persist
import           Database.Persist.MySQL
import qualified Geo.Computations              as GEO
import qualified Geo.Types                     as GEO ()
import           RikardCorp.DBConnection
import qualified RikardCorp.Helpers.DateTime   as HD
import qualified RikardCorp.Helpers.Networking as HN
import           RikardCorp.Helpers.Sql        as HS
import           RikardCorp.Types
import           System.FilePath
import           Text.HandsomeSoup
import           Text.XML.HXT.Core             hiding (err, when)

userAgent :: String
userAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:48.0) Gecko/20100101 Firefox/48.0"

data HouseDetailsTable
  = HouseDetailsTable
    { houseDetailsTableHeading :: T.Text
    , houseDetailsTableInfo :: Maybe T.Text
    , houseDetailsTableData :: Maybe [[T.Text]]
    }
    deriving Show

data GisRubricsResponse = GisRubricsResponse
  { gisRubricsResponseCode       :: Int
  , gisRubricsResponseTotal      :: Int
  , gisRubricsResponseRubCount   :: Int
  , gisRubricsResponseFirmCount  :: Int
  , gisRubricsResponseResult     :: [GisMainRubric]
  } deriving Show

data GisMainRubric = GisMainRubric
  { gisMainRubricId         :: T.Text
  , gisMainRubricName       :: T.Text
  , gisMainRubricAlias      :: T.Text
  , gisMainRubricChildren   :: [JGisRubric]
  } deriving Show

data JGisRubric = JGisRubric
  { jgisRubricId     :: T.Text
  , jgisRubricName   :: T.Text
  , jgisRubricAlias  :: T.Text
  } deriving Show

data GisFirmsResponse = GisFirmsResponse
  { gisFirmsResponseCode       :: Int
  , gisFirmsResponseResult     :: [JGisFirm]
  } deriving Show

data JGisFirm = JGisFirm
  { jgisFirmId      :: T.Text
  , jgisFirmLon     :: Maybe T.Text
  , jgisFirmLat     :: Maybe T.Text
  , jgisFirmName    :: T.Text
  , jgisFirmHash    :: T.Text
  , jgisFirmAddress :: Maybe T.Text
  } deriving Show

data GisFirmDetailsResponse = GisFirmDetailsResponse
  { gisFirmDetailsResponseCode       :: Int
  , gisFirmDetailsResponseResult     :: JGisFirmAdditions
  } deriving Show

data JGisFirmAdditions = JGisFirmAdditions
  { jgisFirmAdditionsRating        :: Double
  , jgisFirmAdditionsRatingContact :: [JGisFirmAdditionsContact]
  } deriving Show

data JGisFirmAdditionsContact = JGisFirmAdditionsContact
  { jgisFirmAdditionsContactType :: T.Text
  , jgisFirmAdditionsContactValue :: T.Text
  , jgisFirmAdditionsContactAlias :: Maybe T.Text
  } deriving Show

data JGisFirmAdditionsGhostContacts = JGisFirmAdditionsGhostContacts
  { jgisFirmAdditionsGhostContactsName     :: Maybe T.Text
  , jgisFirmAdditionsGhostContactsContacts :: [JGisFirmAdditionsContact]
  } deriving Show

instance FromJSON JGisFirmAdditionsContact where
  parseJSON (Object v) = do
    _type <- v .: "type"
    value <- v .: "value"
    alias <- v .:? "alias"
    return $ JGisFirmAdditionsContact _type value alias
  parseJSON invalid = typeMismatch "JGisFirmAdditionsContact" invalid

instance FromJSON JGisFirmAdditionsGhostContacts where
  parseJSON (Object v) = do
    name     <- v .: "name"
    contacts <- v .: "contacts"
    return $ JGisFirmAdditionsGhostContacts name contacts
  parseJSON invalid = typeMismatch "JGisFirmAdditionsGhostContacts" invalid

instance FromJSON GisFirmDetailsResponse where
  parseJSON (Object v) = do
    code   <- read <$> v .: "response_code"
    result <- v .: "result"
    return $ GisFirmDetailsResponse code result
  parseJSON invalid = typeMismatch "GisFirmDetailsResponse" invalid

instance FromJSON JGisFirmAdditions where
  parseJSON (Object v) = do
    rating <- read <$> v .: "rating"
    ghostC <- v .: "contacts"
    return
      $ JGisFirmAdditions
          rating
          (concatMap jgisFirmAdditionsGhostContactsContacts
            (ghostC :: [JGisFirmAdditionsGhostContacts]))
  parseJSON invalid = typeMismatch "JGisFirmAdditions" invalid

instance ToJSON JGisFirmAdditions where
  toJSON (JGisFirmAdditions rating contacts) =
    object
      [ "rating"   .= rating
      , "contacts" .= contacts
      ]

instance ToJSON JGisFirmAdditionsContact where
  toJSON (JGisFirmAdditionsContact _type value alias) =
    object
      [ "type"  .= _type
      , "value" .= value
      , "alias" .= alias
      ]

instance FromJSON GisFirmsResponse where
  parseJSON (Object v) = do
    code      <- read <$> v .: "response_code"
    result    <- v .: "result"
    return $ GisFirmsResponse code result
  parseJSON invalid = typeMismatch "GisFirmsResponse" invalid

instance FromJSON JGisFirm where
  parseJSON (Object v) = do
    _id     <- v .: "id"
    lon     <- v .:? "lon"
    lat     <- v .:? "lat"
    name    <- v .: "name"
    hash    <- v .: "hash"
    address <- v .:? "address"
    return $ JGisFirm _id lon lat name hash address
  parseJSON invalid = typeMismatch "JGisFirm" invalid

instance FromJSON GisRubricsResponse where
  parseJSON (Object v) = do
    code      <- read <$> v .: "response_code"
    total     <- v .: "total"
    rubcount  <- v .: "rubric_count"
    firmcount <- v .: "firm_count"
    result    <- v .: "result"
    return $ GisRubricsResponse code total rubcount firmcount result
  parseJSON invalid = typeMismatch "GisRubricsResponse" invalid

instance FromJSON GisMainRubric where
  parseJSON (Object v) = do
    _id      <- v .: "id"
    name     <- v .: "name"
    alias    <- v .: "alias"
    children <- v .: "children"
    return $ GisMainRubric _id name alias children
  parseJSON invalid = typeMismatch "GisMainRubric" invalid

instance FromJSON JGisRubric where
  parseJSON (Object v) = do
    __id     <- v .: "id"
    name     <- v .: "name"
    alias    <- v .: "alias"
    return $ JGisRubric __id name alias
  parseJSON invalid = typeMismatch "JGisRubric" invalid

type GisFirmsMap = HT.CuckooHashTable T.Text JGisFirm
type GisFirmAdditionsMap = HT.CuckooHashTable T.Text T.Text

main :: IO ()
-- main = putStrLn "Migration complete"
main = getHousesGeo
-- main = migrateGeoMOAO
-- main = showGeoAO
-- main = showStreets
-- main = checkStreets
-- main = fillStreets False
-- main = fillStreets True
-- main = cleanHouses
-- main = cleanStreets
-- main = setHousesDetailsUrls
-- main = dbConn $ runMigration migrateRikardCorpDB
-- main = setHouseGeoByAddr "e6495dbc-16d7-409f-ab0d-add383bf06fa" "Айвазовского ул, 1 к1"
-- main = loadDatasetById 1682
-- main = mapM_
--         loadDatasetById
--         [ 1497 -- Земельные участки в собственности города Москвы и неразграниченной собственности
--         , 1927 -- Адресный реестр объектов недвижимости города Москвы
--         , 674 -- Реестр искусственных дорожных неровностей
--         , 1496 -- Бахчевые развалы
--         , 2454 -- Родники в пределах установленных границ города Москвы
--         , 620 -- Ярмарки выходного дня
--         , 2357 -- Столичные аптеки, реализующие приоритетные городские программы здравоохранения
--         , 2624 -- Религиозные объекты Русской православной церкви
--         , 2601 -- Данные по промышленным предприятиям
--         , 2681 -- Перечень управляющих компаний и товариществ собственников жилья
--         , 1903 -- Общественное питание в Москве
--         , 1047 -- Транспортно-пересадочные узлы
--         , 1488 -- Станции Московского метрополитена
--         , 624 -- Входы и выходы вестибюлей станций Московского метрополитена
--         , 745 -- Пешеходные тоннели
--         , 886 -- Поля футбольные
--         , 629 -- Спортивные объекты города Москвы
--         , 1464 -- Коворкинги
--         , 1804 -- Магазины «Хозяйственные товары»
--         , 1808 -- Универмаги
--         , 1774 -- Сауны
--         , 1851 -- Прочие специализированные непродовольственные предприятия торговли
--         , 1828 -- Минимаркеты
--         , 1806 -- Гипермаркеты (непродовольственные)
--         , 1769 -- Комплексные предприятия бытового обслуживания
--         , 1768 -- Парикмахерские и косметические услуги
--         , 1765 -- Ремонт, окраска и пошив обуви
--         , 1844 -- Торговые центры (аутлет-центры, моллы, ритейл-парки)
--         , 1826 -- Магазины «Бутик»
--         , 1803 -- Магазины «Спорт и туризм»
--         , 1779 -- Химическая чистка и крашение
--         , 1767 -- Услуги бань
--         , 1858 -- Товары для молодежи
--         , 1854 -- Магазины «Алкогольные напитки»
--         , 586 -- Объекты розничной торговли и общественного питания, АЛКО
--         , 1904 -- Бытовые услуги на территории Москвы
--         , 861 -- Перечень открытых Wi-Fi-точек, установленных в пределах парков
--         , 1095 -- Отделения почтовой связи
--         , 981 -- Фонтаны
--         , 1903 -- Общественное питание в Москве
--         , 1796 -- Бары
--         , 1788 -- Рестораны
--         , 1793 -- Столовые
--         , 1792 -- Кафе
--         , 1465 -- Парковые территории
--         , 1661 -- Общественные пункты охраны порядка
--         , 512 -- Молочные кухни
--         , 509 -- Женские консультации
--         , 503 -- Поликлиническая помощь взрослым
--         , 505 -- Поликлиническая помощь детям
--         , 2263 -- Образовательные учреждения города Москвы
--         , 622 -- Перехватывающие парковки
--         , 752 -- Остановки наземного городского пассажирского транспорта
--         , 2354 -- Участковые пункты полиции
--         , 1682 -- Зоны платной парковки
--         , 623 -- Платные парковки на улично-дорожной сети
--         , 2704 -- Военторги
--         , 1022 -- Нежилые помещения, зарегистрированные в собственность города Москвы
--         ]
-- main = updateFirms
-- main = testFirms
-- main = dbConn $ runMigration migrateRikardCorpDB
-- main = getEmptyStreets
-- main = getEmptyStreetsJustGEO
-- main =
--   runResourceT $ runNoLoggingT $ withMySQLConn rikardCorpDBci $ runSqlConn $ do
--     runMigration migrateRikardCorpDB

-- test2GisResponse :: T.Text
-- test2GisResponse = "{\"api_version\":\"1.3\",\"response_code\":\"200\",\"total\":\"1\",\"result\":[{\"id\":\"4504338361745786\",\"project_id\":32,\"type\":\"street\",\"name\":\"Москва, Севастопольский проспект\",\"short_name\":\"Севастопольский проспект\",\"selection\":\"MULTILINESTRING((37.610097 55.6951,37.609079 55.693997,37.607161 55.691919,37.603749 55.688239,37.596401 55.680275,37.596298 55.68017,37.592667 55.676791,37.592217 55.676397,37.580879 55.66584,37.578742 55.663866,37.577641 55.66284,37.565735 55.651744,37.565607 55.651618,37.565486 55.651483,37.564364 55.650204,37.564278 55.650135,37.564182 55.650091,37.563766 55.649957),(37.610238 55.695058,37.60922 55.693956,37.607297 55.691872,37.60389 55.688198,37.59654 55.680231,37.596436 55.680126,37.592803 55.676744,37.592391 55.676337,37.581059 55.665784,37.578918 55.663807,37.574836 55.660004,37.565904 55.65168,37.565788 55.651566,37.565668 55.65143,37.564545 55.650153,37.564503 55.650054,37.564497 55.649946,37.564625 55.649702),(37.564255 55.649145,37.56409 55.649099,37.563742 55.648985,37.563619 55.648926,37.56354 55.648861,37.563456 55.648771,37.561105 55.645678,37.560977 55.645534,37.560686 55.645226,37.560418 55.64498,37.559993 55.644661,37.55963 55.644424,37.559262 55.644199,37.558803 55.643953,37.558346 55.643747,37.557897 55.64357,37.557318 55.643367,37.556583 55.643135,37.550437 55.641268,37.549808 55.641062,37.549368 55.640892,37.548879 55.640683,37.548421 55.640486,37.546881 55.639781,37.546548 55.63962,37.546227 55.639443,37.545984 55.639287,37.545779 55.639133,37.545544 55.638941,37.545314 55.638738,37.545069 55.638494,37.544589 55.637945,37.544082 55.637385,37.543228 55.635902,37.542522 55.63478,37.540737 55.63195,37.540637 55.631775,37.540544 55.631595,37.540423 55.631344,37.540342 55.631127,37.540271 55.630892,37.540224 55.630714,37.540178 55.630458,37.540153 55.630269,37.540149 55.630105,37.540167 55.629897,37.540204 55.629701,37.542741 55.62344,37.543503 55.622254,37.54361 55.622059,37.543684 55.621874,37.543736 55.621686,37.543765 55.621493,37.543777 55.621318,37.543772 55.62117,37.543741 55.621012,37.54367 55.620794,37.543102 55.619366),(37.542311 55.619805,37.542577 55.620323,37.542652 55.620508,37.542695 55.620639,37.542722 55.620795,37.542721 55.620938,37.542715 55.621089,37.542696 55.62125,37.542583 55.622567,37.542464 55.623564,37.539882 55.629944,37.539817 55.630078,37.53973 55.630307,37.539654 55.630547,37.539607 55.630745,37.539591 55.631173,37.539608 55.631411,37.539656 55.631634,37.539832 55.63222,37.539939 55.632472,37.540062 55.632699,37.540212 55.63291,37.540372 55.633108,37.540856 55.633658,37.541328 55.634195,37.542791 55.635981,37.543897 55.637431,37.544411 55.638009,37.544876 55.638543,37.545127 55.638793,37.545362 55.639,37.545597 55.639194,37.545815 55.639356,37.546065 55.639516,37.546397 55.639699,37.546748 55.63987,37.548284 55.640572,37.548749 55.640772,37.549224 55.640975,37.549692 55.641156,37.550342 55.641369,37.556484 55.643235,37.557212 55.643465,37.557784 55.643665,37.55822 55.643837,37.558663 55.644037,37.559114 55.644278,37.559475 55.644499,37.559828 55.64473,37.560241 55.64504,37.560507 55.645284,37.560741 55.645529,37.560916 55.645726,37.563267 55.648817,37.563304 55.648907,37.563313 55.649003,37.563226 55.649359))\",\"centroid\":\"POINT(37.585528 55.669946)\",\"attributes\":{\"city\":\"Москва\",\"rank\":386}}]}"

-- ["LINESTRING","MULTILINESTRING","MULTIPOLYGON","POINT","POLYGON"]

-- testFirms :: IO ()
-- testFirms = do
--   let
--    testText = "{\"api_version\":\"1.3\",\"response_code\":\"200\",\"what\":\"CD DVD BD\",\"where\":\"Москва\",\"total\":\"804\",\"did_you_mean\":{\"rubrics\":[{\"name\":\"CD DVD BD\"}]},\"result\":[{\"id\":\"4504127918214284\",\"lon\":\"37.658907\",\"lat\":\"55.757684\",\"name\":\"1С-Интерес, сеть мультимедийных супермаркетов\",\"firm_group\":{\"id\":\"4504136498397309\",\"count\":\"11\"},\"hash\":\"37edo306G44858A48381G2G15fE23e44G4G6G7I2G12H0J56fEpy397I2898G47044896854x8ufuv57474C243A7I1484C9H1GJec\",\"city_name\":\"Москва\",\"project_id\":\"32\",\"address\":\"Земляной Вал, 33\",\"rubrics\":[\"CD DVD BD\",\"Продажа программного обеспечения\",\"Бухгалтерские программы\",\"Настольные игры\",\"Сувениры\"]}]}"
--   case decodeE @GisFirmsResponse testText of
--     Right resp -> print resp
--     Left err -> error err

updateFirms :: IO ()
updateFirms = do
  (Just apiKey) <- fmap T.pack <$> get2GisApiKey
  updateFirmsWithKey apiKey

updateFirmsWithKey :: T.Text -> IO ()
updateFirmsWithKey apiKey = do
  firms <- dbConn $ selectList [GisFirmAdditions ==. Nothing] []
  firmsHash <-
    map ((gisFirmGisId . entityVal) &&& (fromJust . gisFirmAdditions . entityVal))
    <$> dbConn (selectList [GisFirmAdditions !=. Nothing] [])
  additionsHash <- HT.fromList firmsHash
  forM_ firms $ \firm ->
    updateFirmsWithKeyWithFirm apiKey firm additionsHash

updateFirmsWithKeyWithFirm
  :: T.Text -> Entity GisFirm -> GisFirmAdditionsMap -> IO ()
updateFirmsWithKeyWithFirm apiKey (Entity firmId firm) additionsHash = do
  let firmGisId   = gisFirmGisId firm
      firmGisHash = gisFirmHash firm
  TIO.putStrLn $ firmGisId <> " " <> firmGisHash
  addition' <- HT.lookup additionsHash firmGisId
  addition <-
    maybe
      (getFirmAdditionsWithKeyWithFirm
          apiKey
          firmGisId
          firmGisHash
          additionsHash)
      (\a -> putStrLn "from cache" >> return a)
      addition'
  dbConn $ update firmId [GisFirmAdditions =. Just addition]

getFirmAdditionsWithKeyWithFirm :: T.Text -> T.Text -> T.Text -> GisFirmAdditionsMap -> IO T.Text
getFirmAdditionsWithKeyWithFirm apiKey firmGisId firmGisHash additionsHash = do
  let turl =
        T.unpack $
        HN.renderUrl
          "http://catalog.api.2gis.ru"
          ["profile"]
          [ ("key", apiKey)
          , ("version", "1.3")
          , ("id", firmGisId)
          , ("hash", firmGisHash)
          ]
  jsonResponse <- snd <$> HN.getContentsByURL userAgent turl
  threadDelay 100000
  let
    isJson40x   =
         T.isInfixOf "\"response_code\":\"404\"" (TE.decodeUtf8 jsonResponse)
      || T.isInfixOf "\"response_code\":\"400\"" (TE.decodeUtf8 jsonResponse)
  if isJson40x
    then error "40x"
    else do
      let
        gisR :: Either String JGisFirmAdditions
        gisR = eitherDecode' (BL.fromStrict jsonResponse)
      case gisR of
        Right resp -> do
          let additions = TE.decodeUtf8 . BL.toStrict . encode $ resp
          HT.insert additionsHash firmGisId additions
          return additions
        Left err -> error err

insertFirms :: IO ()
insertFirms = do
  (Just apiKey) <- fmap T.pack <$> get2GisApiKey
  insertFirmsWithKey apiKey

insertFirmsWithKey :: T.Text -> IO ()
insertFirmsWithKey apiKey = do
  rubrics <- dbConn $ selectList ([] :: [Filter GisRubric]) []
  firmsHash <- HT.new
  forM_ rubrics $ \rubric ->
    insertFirmsWithKeyWithRubric apiKey rubric firmsHash

insertFirmsWithKeyWithRubric :: T.Text -> Entity GisRubric -> GisFirmsMap -> IO ()
insertFirmsWithKeyWithRubric apiKey (Entity rubId rubric) _firmsHash = do
  let rubricName = gisRubricName rubric
  TIO.putStrLn rubricName
  firms' <-
        filter corrFirms . concat
    <$> getFirmsWithKeyWithRubric apiKey rubricName 1
  -- firms <- filterM (
  --   \f -> isNothing <$> HT.lookup firmsHash (jgisFirmId f)
  --     ) firms'
  let firms = firms'
  putStrLn $ (show . length $ firms) <> " / " <> (show . length $ firms')
  dbConn $
    forM_ firms $ \f ->
      insert_ $
        GisFirm
          rubId
          (jgisFirmId f)
          (fromJust . jgisFirmLon $ f)
          (fromJust . jgisFirmLat $ f)
          (jgisFirmName f)
          (jgisFirmHash f)
          (fromJust . jgisFirmAddress $ f)
          Nothing
  -- forM_ firms $ \f -> HT.insert firmsHash (jgisFirmId f) f

corrFirms :: JGisFirm -> Bool
corrFirms f =
     isJust (jgisFirmLon f)
  && isJust (jgisFirmLat f)
  && isJust (jgisFirmAddress f)

getFirmsWithKeyWithRubric :: T.Text -> T.Text -> Int -> IO [[JGisFirm]]
getFirmsWithKeyWithRubric apiKey rubricName page = do
  let turl =
        T.unpack $
        HN.renderUrl
          "http://catalog.api.2gis.ru"
          ["search"]
          [ ("where", "Москва")
          , ("key", apiKey)
          , ("version", "1.3")
          , ("what", rubricName)
          , ("sort", "name")
          , ("pagesize", "50")
          , ("page", T.pack . show $ page)
          ]
  jsonResponse <- snd <$> HN.getContentsByURL userAgent turl
  threadDelay 200000
  let
    isJson40x   =
         T.isInfixOf "\"response_code\":\"404\"" (TE.decodeUtf8 jsonResponse)
      || T.isInfixOf "\"response_code\":\"400\"" (TE.decodeUtf8 jsonResponse)
  if isJson40x
    then return []
    else do
      let
        gisR :: Either String GisFirmsResponse
        gisR = eitherDecode' (BL.fromStrict jsonResponse)
      case gisR of
        Right resp ->
          (:) (gisFirmsResponseResult resp)
          <$> getFirmsWithKeyWithRubric apiKey rubricName (page + 1)
        Left err -> error err

insertGisRubrics :: IO ()
insertGisRubrics = do
  (Just apiKey) <- fmap T.pack <$> get2GisApiKey
  let turl =
        T.unpack $
        HN.renderUrl
          "http://catalog.api.2gis.ru"
          ["rubricator"]
          [ ("where", "Москва")
          , ("key", apiKey)
          , ("version", "1.3")
          , ("show_children", "1")
          , ("sort", "name")
          ]
  putStrLn turl
  jsonResponse <- snd <$> HN.getContentsByURL userAgent turl
  let
    isJson404   =
      T.isInfixOf "\"response_code\":\"404\"" (TE.decodeUtf8 jsonResponse)
  when isJson404
    $ TIO.putStrLn "-- NotFound"
  unless isJson404
    $ do
      let
        gisR :: Either String GisRubricsResponse
        gisR = eitherDecode' (BL.fromStrict jsonResponse)
      case gisR of
        Right resp -> do
          let rubrics =
                  DL.sortBy (O.comparing jgisRubricName)
                . DL.concatMap gisMainRubricChildren
                . gisRubricsResponseResult
                $ resp
          dbConn $
            forM_ rubrics $ \r ->
              insert_ $
                GisRubric
                  (jgisRubricId r)
                  (jgisRubricName r)
                  (jgisRubricAlias r)
        Left err -> putStrLn err

dumpGisTowns :: IO ()
dumpGisTowns = do
  (_, content) <- HN.getContentsByURL userAgent "https://2gis.ru"
  putStrLn $ SU8.toString content

storeSadovoeTtkMkad :: IO ()
storeSadovoeTtkMkad = do
  sadovoe <- B.readFile ("data" </> "sadovoe-geo.json")
  ttk <- B.readFile ("data" </> "ttk-geo.json")
  mkad <- B.readFile ("data" </> "mkad-geo.json")
  cDay <- HD.getCurrentUTCDay
  dbConn $ do
    insert_ $
      ForeignGeoObject
        "sadovoe-geo"
        1
        1
        "Границы садового кольца"
        sadovoe
        cDay
    insert_ $
      ForeignGeoObject
        "ttk-geo"
        1
        1
        "Границы третьего транспортного кольца"
        ttk
        cDay
    insert_ $
      ForeignGeoObject
        "mkad-geo"
        1
        1
        "Границы МКАД"
        mkad
        cDay

convertMVDDistrictToJSon :: IO ()
convertMVDDistrictToJSon = do
  content <- B.readFile ("convert" </> "mvd-district.xml")
  let doc :: IOSArrow XmlTree XmlTree
      doc = readString
        [withParseHTML no, withWarnings no, withInputEncoding utf8]
        (SU8.toString content)
  districts <- runX $ extractMVDDistricts doc
  -- print districts
  -- forM_ (head districts) print
  storeToDB (DL.filter ((==) "77" . T.take 2 . mvDistrictSubject) . head $ districts)

storeToDB :: [MVDistrict] -> IO ()
storeToDB mvds = do
  cDay <- HD.getCurrentUTCDay
  dbConn $
    insert_ $
      ForeignGeoObject
        "mvdistricts"
        1
        (DL.length mvds)
        "Перечень территориальных органов МВД России на региональном уровне"
        (BL.toStrict . encode $ mvds)
        cDay
  putStrLn "storeToDB: insert"

extractMVDDistricts :: IOSArrow XmlTree XmlTree -> IOSArrow XmlTree [MVDistrict]
extractMVDDistricts doc =
  doc
    >>> css "value-district"
    >>> proc aBlock -> do
      dstrs <- listA (deep (hasNameWith (\qn ->
        let dgts = DL.drop 1 . DL.dropWhile (/= '-') . localPart $ qn
        in not (DL.null dgts) && all C.isDigit dgts))
        >>> extractDistrict) -< aBlock
      returnA -< dstrs

extractDistrict :: IOSArrow XmlTree MVDistrict
extractDistrict =
    proc aBlock -> do
      name <- ((deep (hasName "value-name") /> getText) `orElse` arr (const "nil")) >>^ T.pack -< aBlock
      subj <- ((deep (hasName "value-subject") /> getText) `orElse` arr (const "nil")) >>^ T.pack -< aBlock
      lat <- ((deep (hasName "value-lat") /> getText) `orElse` arr (const "nil")) >>^ T.pack -< aBlock
      lng <- ((deep (hasName "value-lng") /> getText) `orElse` arr (const "nil")) >>^ T.pack -< aBlock
      str <- ((deep (hasName "value-street") /> getText) `orElse` arr (const "nil")) >>^ T.pack -< aBlock
      house <- ((deep (hasName "value-adres") /> getText) `orElse` arr (const "nil")) >>^ T.pack -< aBlock
      returnA -< (MVDistrict name subj lat lng str house)

loadDatasetById :: Int -> IO ()
loadDatasetById dataId = do
  let dataIdTxt = T.pack $ show dataId
  putStrLn $ "## Dataset " <> show dataId
  cDay <- HD.getCurrentUTCDay
  ex <- dbConn $
    selectList [GeoObjectDataId ==. dataIdTxt] [] --, GeoObjectDate ==. cDay
  unless (null ex) $ putStrLn "skip"
  when (null ex) $ do
    desc <- loadDatasetDesc dataId
    size <- getDatasetCount dataId
    putStrLn $ "size: " <> show size
    jsonData <-
      if size > 10000
        then loadBigDataset size dataId
        else loadSmallDataset dataId
    dbConn $ insert_ $
      GeoObject
        dataIdTxt
        1
        size
        desc
        jsonData
        cDay
    putStrLn "insert"
  putStrLn ""

loadBigDataset :: Int -> Int -> IO B.ByteString
loadBigDataset size dataId = do
  let maxsz = 500 :: Int
      cnt = size `div` maxsz
      rst = size `rem` maxsz
      pgs' = DL.unfoldr
              (
                \cnt' ->
                if cnt' > cnt
                  then Nothing
                  else Just (
                              (
                              cnt' * maxsz
                              , if cnt' == cnt
                                  then rst
                                  else maxsz
                              )
                            , cnt' `seq` cnt' + 1
                            )
              ) (0 :: Int)
      pgs =
        if rst == 0
          then DL.init pgs'
          else pgs'
  jsonData <- B.intercalate (TE.encodeUtf8 ",")
          .   map (B.init . B.tail)
          <$> mapM (loadDatasetByPage dataId) pgs
  let jsonData' =
           TE.encodeUtf8 "["
        <> jsonData
        <> TE.encodeUtf8 "]"
  return jsonData'

loadDatasetByPage :: Int -> (Int, Int) -> IO B.ByteString
loadDatasetByPage dataId (skip, top) = do
  let turl =
        T.unpack $
        HN.renderUrl
          "http://api.data.mos.ru"
          [ "v1"
          , "datasets"
          , T.pack . show $ dataId
          , "rows"
          ]
          [ ("$skip", T.pack . show $ skip)
          , ("$top", T.pack . show $ top)
          ]
  snd <$> HN.getContentsByURL userAgent turl

loadSmallDataset :: Int -> IO B.ByteString
loadSmallDataset dataId = do
  let turl =
        T.unpack $
        HN.renderUrl
          "http://api.data.mos.ru"
          [ "v1"
          , "datasets"
          , T.pack . show $ dataId
          , "rows"
          ]
          []
  snd <$> HN.getContentsByURL userAgent turl

loadDatasetDesc :: Int -> IO T.Text
loadDatasetDesc dataId = do
  let turl =
        T.unpack $
        HN.renderUrl
          "http://api.data.mos.ru"
          [ "v1"
          , "datasets"
          , T.pack . show $ dataId
          ]
          []
  TE.decodeUtf8 . snd <$> HN.getContentsByURL userAgent turl

getDatasetCount :: Int -> IO Int
getDatasetCount dataId = do
  let turl =
        T.unpack $
        HN.renderUrl
          "http://api.data.mos.ru"
          [ "v1"
          , "datasets"
          , T.pack . show $ dataId
          , "count"
          ]
          []
  read . SU8.toString . snd <$> HN.getContentsByURL userAgent turl

downloadDetailsText :: IO ()
downloadDetailsText = do
  streets <-
        map entityVal
    <$> dbConn (selectList [] [Asc AddrStreetId])
  -- streets' <- map entityVal <$> (dbConn . selectList [] $ [Asc AddrStreetId])
  -- let
  --   streets = catMaybes [specifyStreet streets']
  allHouses <-
        map (entityKey &&& entityVal)
    <$> dbConn (selectList
                  [ AddrHouseDetailsUrl !=. Nothing
                  , AddrHouseDetailsText ==. Nothing
                  ]
                  [Asc AddrHouseId])
  forM_ streets
    $ \AddrStreet
      { addrStreetGuid       = guid
      } -> do
        let houses =
              filter ((==) guid . addrHouseGuid . snd) allHouses
        forM_ houses
          $ \(houseId,
            AddrHouse
              { addrHouseDetailsUrl = url
              , addrHouseHouseGuid = houseGUID
              }) -> do
                let startUrl = fromJust url
                -- let startUrl = "/Building/Details/b2e9df02-55f0-4e13-ad67-3fe71d4dc068"
                (doc, cookies) <- downloadDetailsContent Nothing startUrl
                redirectUrls <- extractDetailsTextWithRedirectUrl doc
                -- print redirectUrls
                let urls = fromMaybe [startUrl] redirectUrls
                houseInfos <- mapM (getHouseInfo houseGUID cookies) urls
                -- TIO.putStrLn $ TE.decodeUtf8 . BL.toStrict . encode $ houseInfos
                -- error "____________"
                dbConn
                  $ do
                    let houseInfoJson =
                          TE.decodeUtf8 . BL.toStrict . encode $ houseInfos
                    update houseId [AddrHouseDetailsText =. Just houseInfoJson]
                TIO.putStrLn houseGUID

getHouseInfo :: T.Text -> HN.Cookies -> T.Text -> IO HouseInfo
getHouseInfo houseGUID cookies url = do
  (detailsDoc, _) <-
    downloadDetailsContent cookies url
  (extraUrl, mainDetails) <-
    extractDetailsTextWithMainUrl detailsDoc
  extraDetails <-
    maybe (return [])
      (downloadDetailsTextWithExtraUrl cookies) extraUrl
  -- mapM_ showHouseDetailsTable mainDetails
  -- putStrLn "---------------------------------"
  -- mapM_ showHouseDetailsTable extraDetails
  -- putStrLn "#################################"
  when ( DL.null mainDetails
      || all (isNothing . houseDetailsTableData) mainDetails
      || (
          (  DL.null extraDetails
          || all (isNothing . houseDetailsTableData) extraDetails
          )
          && isJust extraUrl
         )
       )
    $ error (T.unpack houseGUID)
  let houseInfo :: HouseInfo
      houseInfo
        = HouseInfo
            (getHouseInfoStewardship mainDetails)
            (getHouseInfoMain mainDetails)
            (getHouseInfoExtra extraDetails)
  return houseInfo

getHouseInfoExtra :: [HouseDetailsTable] -> Maybe HouseExtraInfo
getHouseInfoExtra [] = Nothing
getHouseInfoExtra (hdt:hdts) =
  if houseDetailsTableHeading hdt == "Общие сведения о многоквартирном доме"
    then Just HouseExtraInfo
          { houseExtraInfoData = fillData . houseDetailsTableData $ hdt
          }
    else getHouseInfoExtra hdts
  where
    fillData :: Maybe [[T.Text]] -> Maybe (M.Map T.Text (M.Map T.Text T.Text))
    fillData Nothing = Nothing
    fillData (Just ts) =
      let dataMap =
              M.fromList
            . makeMapList
            . DL.groupBy ((==) `on` DL.length)
            . DL.filter (not . DL.null)
            $ ts
      in
      if M.null dataMap
        then Nothing
        else Just dataMap

    makeMapList :: [[[T.Text]]] -> [(T.Text, M.Map T.Text T.Text)]
    makeMapList [] = []
    makeMapList (k:v:tss)
      = let hhk = DL.length . head $ k
            hhv = DL.length . head $ v
        in
          if hhk == hhv
            then makeMapList (v:tss)
            else
                (T.strip (head . head $ k),
                M.fromList $ makeMapFromList v)
              : makeMapList tss
    makeMapList (_:tss)
      = makeMapList tss

getHouseInfoMain :: [HouseDetailsTable] -> Maybe HouseMainInfo
getHouseInfoMain [] = Nothing
getHouseInfoMain (hdt:hdts) =
  if houseDetailsTableHeading hdt == "Информация о доме"
    then Just HouseMainInfo
          { houseMainInfoData = fillData . houseDetailsTableData $ hdt
          }
    else getHouseInfoMain hdts
  where
    fillData :: Maybe [[T.Text]] -> Maybe (M.Map T.Text T.Text)
    fillData Nothing = Nothing
    fillData (Just ts) =
      let dataMap =
              M.fromList
            . makeMapFromList
            . DL.filter (not . DL.null)
            $ ts
      in
      if M.null dataMap
        then Nothing
        else Just dataMap

makeMapFromList :: [[T.Text]] -> [(T.Text, T.Text)]
makeMapFromList [] = []
makeMapFromList ([k,v]:tss)
  =
    (
        T.strip
      . T.dropWhile (== '-')
      . fromMaybe k
      . T.stripSuffix ":"
      $ k
    , T.strip v
    )
  : makeMapFromList tss
makeMapFromList (_:tss)
  = makeMapFromList tss

getHouseInfoStewardship :: [HouseDetailsTable] -> Maybe HouseStewardship
getHouseInfoStewardship [] = Nothing
getHouseInfoStewardship (hdt:hdts) =
  if houseDetailsTableHeading hdt == "Управление домом"
    then Just HouseStewardship
          { houseStewardshipData = fillData . houseDetailsTableData $ hdt
          , houseStewardshipInfo = houseDetailsTableInfo hdt
          }
    else getHouseInfoStewardship hdts
  where
    fillData :: Maybe [[T.Text]] -> Maybe (M.Map T.Text (S.Set T.Text))
    fillData Nothing = Nothing
    fillData (Just ts) =
      let dataMap =
              M.fromList
            . makeMapList
            . DL.filter (not . DL.null)
            . DL.groupBy ((==) `on` T.isSuffixOf ":")
            . DL.concat
            $ ts
      in
      if M.null dataMap
        then Nothing
        else Just dataMap

    makeMapList :: [[T.Text]] -> [(T.Text, S.Set T.Text)]
    makeMapList [] = []
    makeMapList (ts1:ts2:tss)
      =
        (
          let t = head ts1
          in T.strip (fromMaybe t (T.stripSuffix ":" t))
        , S.fromList . map T.strip $ ts2
        )
      : makeMapList tss
    makeMapList (_:tss)
      = makeMapList tss


downloadDetailsContent :: HN.Cookies -> T.Text -> IO (IOSArrow XmlTree XmlTree, HN.Cookies)
downloadDetailsContent rqcookies detailsUrl = do
  (_, content, cookies) <-
    HN.getContentsWithCookiesByURL
      rqcookies
      userAgent
      ("http://dom.mos.ru" <> T.unpack detailsUrl)
  let doc :: IOSArrow XmlTree XmlTree
      doc = readString
        [withParseHTML yes, withWarnings no, withInputEncoding utf8]
        (SU8.toString content)
  return (doc, cookies)

extractDetailsTextWithRedirectUrl :: IOSArrow XmlTree XmlTree -> IO (Maybe [T.Text])
extractDetailsTextWithRedirectUrl doc = do
  redirectUrl <- runX $ extractHouseRedirectUrl doc
  return
    $ if DL.null redirectUrl
      then Nothing
      else head redirectUrl

extractDetailsTextWithMainUrl :: IOSArrow XmlTree XmlTree -> IO (Maybe T.Text, [HouseDetailsTable])
extractDetailsTextWithMainUrl doc = do
  hdts <- runX $ extractHouseMainDetails doc
  extraUrl <- runX $ extractHouseExtraDetailsUrl doc
  return (if DL.null extraUrl then Nothing else head extraUrl, hdts)

downloadDetailsTextWithExtraUrl :: HN.Cookies -> T.Text -> IO [HouseDetailsTable]
downloadDetailsTextWithExtraUrl cookies detailsExtraUrl = do
  (_, content, _) <-
    HN.getContentsWithCookiesByURL
      cookies
      userAgent
      ("http://dom.mos.ru" <> T.unpack detailsExtraUrl)
  let doc :: IOSArrow XmlTree XmlTree
      doc = readString
        [withParseHTML yes, withWarnings no, withInputEncoding utf8]
        (SU8.toString content)
  runX $ extractHouseMainDetails doc

showHouseDetailsTable :: HouseDetailsTable -> IO ()
showHouseDetailsTable (HouseDetailsTable h1 minfo mls) = do
  TIO.putStrLn h1
  flip (maybe (return ())) minfo
    $ \info -> TIO.putStrLn info
  flip (maybe (return ())) mls
    $ \ls ->
      forM_ ls
        $ \row ->
          TIO.putStrLn . T.unwords $ row

extractHouseRedirectUrl :: IOSArrow XmlTree XmlTree -> IOSArrow XmlTree (Maybe [T.Text])
extractHouseRedirectUrl doc =
  doc
    >>> css "div[class~=partsInfoBlock]"
    >>> proc aBlock -> do
      ans <- listA (css "a" >>> extractAnchor) -< aBlock
      let redirectUrl =
            filter ((\u -> T.isPrefixOf "/Building/Details?pk=" u
                        || T.isPrefixOf "/Building/Details/"    u) . snd) ans
      returnA -<
        if DL.null redirectUrl
          then Nothing
          else Just $ map snd redirectUrl

extractHouseExtraDetailsUrl :: IOSArrow XmlTree XmlTree -> IOSArrow XmlTree (Maybe T.Text)
extractHouseExtraDetailsUrl doc =
  doc
    >>> proc aBlock -> do
      ans <- listA (css "a" >>> extractAnchor) -< aBlock
      let extraUrl =
            filter ((==) "Общие сведения о многоквартирном доме" . fst) ans
      returnA -< (if DL.null extraUrl then Nothing else Just $ snd (head extraUrl))

extractAnchor :: IOSArrow XmlTree (T.Text, T.Text)
extractAnchor =
  proc aBlock -> do
    href  <- getAttrValue "href" >>^ (T.strip . T.pack) -< aBlock
    aText <- getAllTextMaybe -< aBlock
    returnA -< (fromMaybe T.empty aText, href)

extractHouseMainDetails :: IOSArrow XmlTree XmlTree -> IOSArrow XmlTree HouseDetailsTable
extractHouseMainDetails doc =
  doc
    >>> (css "div[class~=inside-house]" `orElse` css "div#content")
    >>> css "div[class~=rndBrdBlock]"
    >>> proc tblBlock -> do
      h1  <- deep (hasName "h1") /> getText >>^ (T.strip . T.pack) -< tblBlock
      inf <- withDefault extractHouseTableInfo Nothing -< tblBlock
      ls  <- withDefault extractHouseTable Nothing -< tblBlock
      returnA -< HouseDetailsTable h1 inf ls

extractHouseTableInfo :: IOSArrow XmlTree (Maybe T.Text)
extractHouseTableInfo = css "div[class~=partsInfoBlock]" >>> getAllTextMaybe

getAllTextMaybe :: IOSArrow XmlTree (Maybe T.Text)
getAllTextMaybe =
      listA (multi getText)
  >>^ DL.concat
  >>^ (T.strip . T.pack)
  >>^ (\alltxt -> if T.null alltxt then Nothing else Just alltxt)

extractHouseTable :: IOSArrow XmlTree (Maybe [[T.Text]])
extractHouseTable =
  proc tblBlock -> do
    ls <- deep (hasName "table")
          >>>
          listA (
            deep (hasName "tr")
            >>>
            listA
            (
              deep (hasName "td") >>>
                (
                  (deep (hasName "a") /> getText >>^ (T.strip . T.pack))
                  `orElse`
                  (deep (hasName "text") /> getText >>^ (T.strip . T.pack))
                  `orElse`
                  (listA (multi getText) >>^ DL.concat >>^ (T.strip . T.pack))
                )
            )
          ) -< tblBlock
    returnA -<
        Just
      . filter (not . DL.null)
      . filter (not . all T.null)
      $ ls

testPoint :: T.Text
testPoint = "POINT(37.542412 55.618028)"

testLinestring :: T.Text
testLinestring = "LINESTRING(37.535943 55.521025,37.534745 55.521228,37.534659 55.521233,37.534204 55.521206)"

testMultilinestring :: T.Text
testMultilinestring = "MULTILINESTRING((37.423365 55.66784,37.423158 55.667381,37.423153 55.667271,37.423186 55.667181,37.423037 55.667107,37.422897 55.667032,37.422767 55.666944,37.422238 55.666562,37.42212 55.666482,37.422023 55.666429,37.421916 55.666383,37.421466 55.666227,37.4199 55.665763,37.419656 55.665715,37.419378 55.665676,37.418385 55.66562),(37.423186 55.667181,37.423407 55.667267,37.423712 55.667363,37.426356 55.668104))"

testPolygon :: T.Text
testPolygon = "POLYGON((37.590084 55.500073,37.589007 55.498237,37.585171 55.498977,37.584638 55.499018,37.580392 55.499837,37.580152 55.499803,37.580004 55.499796,37.579855 55.499805,37.579733 55.499828,37.579652 55.499879,37.579307 55.499535,37.579548 55.499447,37.581892 55.498691,37.582059 55.498625,37.582176 55.498567,37.582384 55.498426,37.581097 55.497492,37.580875 55.497568,37.580699 55.497643,37.580322 55.497359,37.579605 55.496777,37.579561 55.496758,37.579519 55.496751,37.582991 55.49569,37.583976 55.495694,37.585134 55.496957,37.585268 55.496952,37.585433 55.496939,37.585598 55.496902,37.587405 55.496244,37.587556 55.496206,37.587742 55.496186,37.587966 55.496197,37.588404 55.496269,37.588631 55.496296,37.587143 55.494675,37.589451 55.49403,37.590614 55.495381,37.590673 55.495325,37.590815 55.495214,37.590991 55.495133,37.59126 55.495069,37.591529 55.495045,37.591823 55.495033,37.592396 55.495044,37.593569 55.495065,37.59378 55.495087,37.594002 55.49513,37.594193 55.495208,37.594349 55.4953,37.594489 55.495409,37.594681 55.495637,37.5932 55.496037,37.594961 55.498134,37.595399 55.498103,37.595392 55.498119,37.595384 55.498186,37.595396 55.498267,37.595594 55.498559,37.595628 55.498584,37.595684 55.498596,37.595749 55.498588,37.5958 55.498571,37.595832 55.498537,37.595852 55.498467,37.596323 55.498482,37.596086 55.498835,37.596086 55.498835,37.595295 55.500033,37.594127 55.499988,37.59318 55.499949,37.592717 55.49994,37.592225 55.499936,37.591672 55.49995,37.591168 55.499976,37.590673 55.500012,37.590084 55.500073))"

testMultiPolygon :: T.Text
testMultiPolygon = "MULTIPOLYGON(((37.796931 56.004625,37.79317 56.005434,37.790794 56.007621,37.794907 56.009298,37.798693 56.009621,37.799173 56.009345,37.798443 56.009091,37.798801 56.008272,37.798655 56.007888,37.797942 56.007654,37.799608 56.006058,37.79738 56.005277,37.796931 56.004625)),((37.790655 56.002493,37.790172 56.002957,37.79322 56.003944,37.79327 56.003935,37.793408 56.003921,37.794348 56.003055,37.792236 56.002364,37.791745 56.002853,37.790655 56.002493)))"

splitStreetLinesTo30Meters :: IO ()
splitStreetLinesTo30Meters = do
  streets <-
        M.fromList
     .  map ((addrStreetGuid . entityVal) &&& (entityKey &&& entityVal))
    <$> dbConn (selectList [] [Asc AddrStreetId])
  let Just street = M.lookup "3a7b8640-f595-4f5a-ad77-70d52f64945a" streets
      Just geoString = addrStreetSelection . snd $ street
      Just geoCoords = parseGEOGisString geoString
      Just (POINT p1) = parseGEOGisString "POINT(37.542949 55.617198)"
      Just (POINT p2) = parseGEOGisString "POINT(37.539885 55.617109)"
      -- MULTILINESTRING cds = splitGEOCdsByMeters 10 geoCoords
      LINESTRING pts = splitGEOCdsByMeters 10 geoCoords
  -- showYandexPoints $ splitPointsByMeters 10 True p1 p2
  -- showYandexPoints $ cds !! 1
  showYandexPoints pts
  print (ceiling (GEO.distance p1 p2) :: Integer)

showYandexPoints :: [GEO.Point] -> IO ()
showYandexPoints pts = do
  forM_ pts
    $ \GEO.Point { GEO.pntLat = lat, GEO.pntLon = lon } ->
      putStr $ "[" ++ show lat ++ ", " ++ show lon ++ "],"
  putStrLn ""

setHouseGeoByAddr :: T.Text -> T.Text -> IO ()
setHouseGeoByAddr guid addr = do
  allHouses <-
        map (entityKey &&& entityVal)
    <$> dbConn (selectList [AddrHouseHouseGuid ==. guid] [Asc AddrHouseId])
  unless (DL.null allHouses)
    $ do
      let house = head allHouses
      (Just apiKey) <- fmap T.pack <$> get2GisApiKey
      when (isNothing . addrHouseCentroid . snd $ house)
        $ getHousesGeoWithAddr apiKey (fst house) addr

getHousesGeo :: IO ()
getHousesGeo = do
  (Just apiKey) <- fmap T.pack <$> get2GisApiKey
  TIO.putStrLn $ "API key: " <> apiKey
  getHousesGeoWithKey apiKey

getHousesGeoWithKey :: T.Text -> IO ()
getHousesGeoWithKey apiKey = do
  streets <-
        map entityVal
    <$> dbConn (selectList [] [Asc AddrStreetId])
  -- streets' <- map entityVal <$> (dbConn . selectList [] $ [Asc AddrStreetId])
  -- let
  --   streets = catMaybes [specifyStreet streets']
  allHouses <-
        map (entityKey &&& entityVal)
    <$> dbConn (selectList [AddrHouseCentroid ==. Nothing] [Asc AddrHouseId])
    -- <$> dbConn (selectList [] [Asc AddrHouseId])
  forM_ streets
    $ \AddrStreet
      { addrStreetGuid       = guid
      , addrStreetOffName    = oName
      } -> do
        let houses =
                filter ((==) guid . addrHouseGuid . snd) allHouses
        forM_ houses
          $ \(houseId,
            AddrHouse
              { addrHouseHouseNum = houseNum
              , addrHouseBuildNum = buildNum
              , addrHouseStrNum   = strNum
              , addrHouseCentroid = houseCentroid
              }) -> do
                let houseN = if T.null houseNum then T.empty else " " <> houseNum
                    buildN = if T.null buildNum then T.empty else " к " <> buildNum
                    strN   = if T.null strNum then T.empty else " ст " <> strNum
                    addr :: T.Text
                    addr = "Москва, " <> case T.unpack oName of
                      "Зеленоград" ->
                        let dNum :: T.Text
                            dNum =
                                T.pack
                              . show
                              . flip div 100
                              . (read :: String -> Int)
                              . T.unpack
                              . (\n -> if T.null n then "0" else n)
                              . T.filter C.isDigit
                              $ if T.null houseNum then buildNum else houseNum
                        in
                        (dNum <> "-й микрорайон")
                          <> ","
                          <> houseN
                          <> buildN
                          <> strN
                      _ ->
                        oName
                          <> ","
                          <> houseN
                          <> buildN
                          <> strN
                when (isNothing houseCentroid)
                  $ getHousesGeoWithAddr apiKey houseId addr

getHousesGeoWithAddr :: T.Text -> AddrHouseId -> T.Text -> IO ()
getHousesGeoWithAddr apiKey houseId addr = do
  let turl =
        T.unpack $
        HN.renderUrl
          "http://catalog.api.2gis.ru"
          ["geo", "search"]
          [ ("q", addr)
          , ("key", apiKey)
          , ("version", "1.3")
          ]
  TIO.putStrLn addr
  jsonResponse <- snd <$> HN.getContentsByURL userAgent turl
  let
    isJson404   =
      T.isInfixOf "\"response_code\":\"404\"" (TE.decodeUtf8 jsonResponse)
  when isJson404
    $ TIO.putStrLn $ "-- NotFound: " <> addr
  unless isJson404
    $ do
        let
          gisR :: Either String GisResponse
          gisR = eitherDecode' (BL.fromStrict jsonResponse)
        case gisR of
          Right resp -> when (gisResponseCode resp == 200) $ do
              TIO.putStrLn $ "++ OK: " <> addr
              dbConn $ do
                let
                  obj = head . gisResponseObjects $ resp
                  cen = gisObjectCentroid obj
                  attrs =
                      TE.decodeUtf8
                    . BL.toStrict
                    . encode
                    $ AddrHouseAddtionsData
                      { gisAttributes = Just (gisObjectAttributes obj)
                      }
                update
                  houseId
                  [ AddrHouseCentroid =. Just cen
                  , AddrHouseAdditions =. Just attrs
                  ]
          Left err -> do
            putStrLn $ "-- error json decoding: " <> err
            TIO.putStrLn (TE.decodeUtf8 jsonResponse)
  -- threadDelay 200000


specifyStreet :: [AddrStreet] -> Maybe AddrStreet
specifyStreet [] = Nothing
specifyStreet (s@AddrStreet { addrStreetGuid = guid }:ss) =
  if guid == "5e5585e7-2105-4d15-84ef-d01f4d2ce91f"
    then
      Just s
    else
      specifyStreet ss

getEmptyStreetsJustGEO :: IO ()
getEmptyStreetsJustGEO = dbConn $ do
  streets <- map entityVal <$> selectList [] [Asc AddrStreetId]
  houses <- map entityVal <$> selectList [] [Asc AddrHouseId]
  forM_ streets
    $ \AddrStreet
      { addrStreetGuid       = guid
      , addrStreetFormalName = fName
      , addrStreetShortName  = sName
      } -> do
        let isJustDetails =
                filter (isJust . addrHouseDetailsUrl)
              . filter ((==) guid . addrHouseGuid)
              $ houses
            isJustCentroid =
                filter (isJust . addrHouseCentroid) isJustDetails

        when (length isJustDetails /= length isJustCentroid)
          $ liftIO
          $ TIO.putStrLn
          $ fName <> " " <> sName

getEmptyStreetsGEO :: IO ()
getEmptyStreetsGEO = dbConn $ do
  streets <- map entityVal <$> selectList [] [Asc AddrStreetId]
  houses <- map entityVal <$> selectList [] [Asc AddrHouseId]
  forM_ streets
    $ \AddrStreet
      { addrStreetGuid       = guid
      , addrStreetFormalName = fName
      , addrStreetShortName  = sName
      } -> do
        let isJustDetails =
                filter (isJust . addrHouseDetailsUrl)
              . filter ((==) guid . addrHouseGuid)
              $ houses
            isJustCentroid =
                filter (isJust . addrHouseCentroid) houses

        when (not (null isJustDetails) && null isJustCentroid)
          $ liftIO
          $ TIO.putStrLn
          $ fName <> " " <> sName

getEmptyStreets :: IO ()
getEmptyStreets = dbConn $ do
  streets <- map entityVal <$> selectList [] [Asc AddrStreetId]
  houses <- map entityVal <$> selectList [] [Asc AddrHouseId]
  forM_ streets
    $ \AddrStreet
      { addrStreetGuid       = guid
      , addrStreetFormalName = fName
      , addrStreetShortName  = sName
      } -> do
        let isAllNothing =
                DL.all isNothing
              . map addrHouseDetailsUrl
              . filter ((==) guid . addrHouseGuid)
              $ houses
        when isAllNothing
          $ liftIO
          $ TIO.putStrLn
          $ fName <> " " <> sName

setHousesDetailsUrls :: IO ()
setHousesDetailsUrls = do
  streets <- map entityVal <$> (dbConn . selectList [] $ [Asc AddrStreetId])
  -- streets' <- map entityVal <$> (dbConn . selectList [] $ [Asc AddrStreetId])
  -- let
  --   streets = catMaybes [specifyStreet streets']
  allHouses <-
        map (entityKey &&& entityVal)
    <$> dbConn
          (selectList
            [AddrHouseDetailsUrl ==. Nothing] [Asc AddrHouseId])
  forM_ streets
    $ \AddrStreet
      { addrStreetGuid       = guid
      , addrStreetFormalName = fName''
      , addrStreetShortName  = sName'
      } -> do
      let fName' = correctStreetFormalName . removeYo $ fName''
          fName = replaceStreet . T.unpack $ fName'
          sName = T.filter ( /= '.') sName'
          turl =
            T.unpack $
            HN.renderUrl
              "http://dom.mos.ru"
              ["Lookups", "GetSearchAutoComplete"]
              [ ("term", fName)
              , ("section", "Buildings")
              ]
      TIO.putStrLn fName
      putStrLn turl
      jsonResponse <- snd <$> HN.getContentsByURL userAgent turl
      let
        domMosR :: Either String [DomMosResult]
        domMosR = eitherDecode' (BL.fromStrict jsonResponse)
      case domMosR of
        Right ress -> do
          let houses =
                filter
                  ((==) guid
                  . addrHouseGuid
                  . snd
                  ) allHouses
          forM_ houses
            $ \(houseId,
              AddrHouse
                { addrHouseHouseNum = houseNum
                , addrHouseBuildNum = buildNum
                , addrHouseStrNum = strNum
                }) -> do
              let houseN = if T.null houseNum then T.empty else ", д." <> houseNum
                  houseNZel = if T.null houseNum then T.empty else ", к." <> T.drop 1 houseNum
                  buildN = if T.null buildNum then T.empty else ", к." <> buildNum
                  strN   = if T.null strNum then T.empty else ", с." <> strNum
                  addr :: T.Text
                  addr = case T.unpack fName of
                    "Зеленоград" ->
                      fName
                        <> " "
                        <> sName
                        <> "."
                        <> houseNZel
                        <> buildN <> strN
                    _ ->
                      if fName' == fName
                        then
                          fName
                            <> " "
                            <> corrShortName (T.unpack sName)
                            <> needDot (T.unpack sName)
                            <> houseN <> buildN <> strN
                        else
                          fName
                            <> houseN
                            <> buildN
                            <> strN
                  url = getHouseUrl ress addr
              case url of
                Just _ -> do
                  TIO.putStrLn addr
                  dbConn $ update houseId [AddrHouseDetailsUrl =. url]
                _ -> return ()
        Left err -> putStrLn err

corrShortName :: String -> T.Text
corrShortName "кв-л" = "квартал"
corrShortName "ш" = "шоссе"
corrShortName "б-р" = "бульв"
corrShortName "пр-кт" = "просп"
corrShortName "проезд" = "пр"
corrShortName "пр-д" = "пр"
corrShortName "ал" = "аллея"
corrShortName "лн" = "линия"
corrShortName name = T.pack name

needDot :: String -> T.Text
needDot "кв-л" = T.empty
needDot "ш" = T.empty
needDot "аллея" = T.empty
needDot "ал" = T.empty
needDot "лн" = T.empty
needDot "просек" = T.empty
needDot "просека" = T.empty
needDot "парк" = T.empty
needDot "городок" = T.empty
needDot "проселок" = T.empty
needDot "линия" = T.empty
needDot _ = "."

replaceStreet :: String -> T.Text
replaceStreet "Волжский Бульвар 114 А" = "Волжский бульвар квартал 114А"
replaceStreet "Волжский Бульвар 113 А" = "Волжский бульвар квартал 113А"
replaceStreet "Волжский Бульвар 95-й" = "Волжский Бульвар квартал 95"
replaceStreet "10-летия Октября" = "Десятилетия Октября ул."
replaceStreet "40 лет Октября" = "Сорок Лет Октября просп."
replaceStreet "50 лет Октября" = "Пятьдесят лет Октября ул."
replaceStreet "60-летия Октября" = "Шестидесятилетия Октября просп."
replaceStreet "Толмачевский Ст." = "Толмачевский Стар. пер."
replaceStreet "Полевая (Дер. Захарьино)" = "Полевая ул. (дер.Захарьино)"
replaceStreet "Центральный Хорошевского Серебряного Бора" = "Центральный пр. Хорошевского Серебряного Бора"
replaceStreet "Нерис Саломеи" = "Саломеи Нерис ул."
replaceStreet "Петровско-Разумовский С." = "Петровско-Разумовский Стар. пр."
replaceStreet "Зыковский С." = "Зыковский Стар. пр."
replaceStreet fName = T.pack fName

correctStreetFormalName :: T.Text -> T.Text
correctStreetFormalName fName =
  let ws = T.words fName
  in
    case head ws of
      "Академика" -> T.unwords . DL.reverse $ ws
      "Авиаконструктора" -> T.unwords . DL.reverse $ ws
      "Матроса" -> T.unwords . DL.reverse $ ws
      "Адмирала" -> T.unwords . DL.reverse $ ws
      "Маршала" -> T.unwords . DL.reverse $ ws
      "Генерала" -> T.unwords . DL.reverse $ ws
      "Летчика" -> T.unwords . DL.reverse $ ws
      "Скульптора" -> T.unwords . DL.reverse $ ws
      "8" -> T.unwords $ "Восьмого" : DL.drop 1 ws
      "9" -> T.unwords $ "Девятого" : DL.drop 1 ws
      "1812" -> T.unwords $ "Тысяча Восемьсот Двенадцатого" : DL.drop 1 ws
      "800-летия" -> T.unwords $ "Восьмисотлетия" : DL.drop 1 ws
      "26-ти" -> T.unwords $ "Двадцати Шести" : DL.drop 1 ws
      _ -> fName

removeYo :: T.Text -> T.Text
removeYo = T.map (\ch -> if ch == 'ё' then 'е' else ch)

getHouseUrl :: [DomMosResult] -> T.Text -> Maybe T.Text
getHouseUrl [] _ = Nothing
getHouseUrl (DomMosResult
  { domMosResultValue = value'
  , domMosResultUrl = url
  }:ress) addr =
  let value =
          T.replace ", д.Б/Н" ""
        . T.replace "д/в." "д."
        $ value'
  in
    if removeSpaces (T.toLower addr) == removeSpaces (T.toLower value)
      then
        Just url
      else
        getHouseUrl ress addr
  where
    removeSpaces :: T.Text -> T.Text
    removeSpaces = T.filter (not . C.isSpace)

cleanStreets :: IO ()
cleanStreets = do
  streets <-
        map (entityKey &&& (addrStreetGuid . entityVal))
    <$> (dbConn . selectList [] $ [Asc AddrStreetId])
  allHouses <-
        S.fromList . map (addrHouseGuid . entityVal)
    <$> (dbConn . selectList [] $ [Asc AddrHouseId])
  dbConn
    $ forM_ streets
        $ \(sId, guid) -> do
          let isHousesExists = S.member guid allHouses
          unless isHousesExists $ delete sId
  return ()

cleanHouses :: IO ()
cleanHouses = do
  streets <-
    S.fromList
      .   map (addrStreetGuid . entityVal)
      <$> (dbConn . selectList [] $ [Asc AddrStreetId])
  dbConn
    $ do
      houses <- selectList ([] :: [Filter AddrHouse]) []
      forM_ houses
        $ \(Entity hId hVal) ->
          unless (S.member (addrHouseGuid hVal) streets)
          $ delete hId
  return ()

checkStreets :: IO ()
checkStreets = do
  streets <-
    dbConn $ selectList [AddrStreetSelection !=. Nothing] [Asc AddrStreetId]
  forM_ streets $ \(Entity sId sVal) -> do
    let
      sameCentroids =
        findSameCentroids sId (fromMaybe "" (addrStreetCentroid sVal)) streets
    unless (DL.null sameCentroids) $ do
      TIO.putStrLn
        $ (T.pack . show $ sqlKeyInt sId)
        <> ": "
        <> addrStreetOffName sVal
      forM_ sameCentroids $ \(Entity sId' sVal') ->
        TIO.putStrLn
          $ "    "
          <>
          (T.pack . show $ sqlKeyInt sId')
          <> " - "
          <> addrStreetOffName sVal'
  return ()

findSameCentroids
  :: AddrStreetId -> T.Text -> [Entity AddrStreet] -> [Entity AddrStreet]
findSameCentroids _ _ [] = []
findSameCentroids tId centr (s@(Entity sId sVal):ss)
  | tId == sId
  = next
  | T.null centr
  = []
  | centr == fromMaybe "" (addrStreetCentroid sVal)
  = s : next
  | otherwise
  = next
  where
    next = findSameCentroids tId centr ss

fillStreets :: Bool -> IO ()
fillStreets isOnlyOffName = do
  streets <-
    dbConn $
    selectList
      [AddrStreetSelection ==. Nothing] [Asc AddrStreetId]
  unless (DL.null streets) $ do
    (Just apiKey) <- fmap T.pack <$> get2GisApiKey
    forM_ streets $ \(Entity sId sVal) -> do
      let
        fName           = addrStreetFormalName sVal
        sName           = addrStreetShortName sVal
        oName           = addrStreetOffName sVal
        streetAddrShort = "Москва, " <> fName <> " " <> sName
        streetAddrFull  = "Москва, " <> oName
        streetAddr =
          if isOnlyOffName
             || T.any (== '.') fName
             || sName == "дор"
             || sName == "кв-л"
            then streetAddrFull
            else streetAddrShort
        turl =
          T.unpack $
          HN.renderUrl
            "http://catalog.api.2gis.ru"
            ["geo", "search"]
            [ ("q", streetAddr)
            , ("key", apiKey)
            , ("version", "1.3")
            ]
      putStrLn turl
      jsonResponse <- snd <$> HN.getContentsByURL userAgent turl
      unless
          (T.isInfixOf "\"response_code\":\"404\"" (TE.decodeUtf8 jsonResponse))
        $ do
            let
              gisR :: Either String GisResponse
              gisR = eitherDecode' (BL.fromStrict jsonResponse)
            case gisR of
              Right resp -> when (gisResponseCode resp == 200) $ do
                  TIO.putStrLn streetAddr
                  dbConn $ do
                    let
                      obj = head . gisResponseObjects $ resp
                      sel = gisObjectSelection obj
                      cen = gisObjectCentroid obj
                      rnk = gisObjectAttributesRank . gisObjectAttributes $ obj
                    update
                      sId
                      [ AddrStreetSelection =. Just sel
                      , AddrStreetCentroid =. Just cen
                      , AddrStreetRank =. rnk
                      ]
              Left err -> do
                putStrLn err
                TIO.putStrLn (TE.decodeUtf8 jsonResponse)
      threadDelay 500000
showStreets :: IO ()
showStreets = do
  streets <-
    dbConn
      $   map
        ( (addrStreetFormalName &&& addrStreetShortName)
        . entityVal
        )
      <$> selectList ([] :: [Filter AddrStreet]) [Asc AddrStreetId]
  forM_ streets $ \(f, s) ->
    TIO.putStrLn $ f <> " " <> s
  -- (Just apiKey) <- fmap T.pack <$> get2GisApiKey
  -- let
  --   turl = HN.renderUrl "http://catalog.api.2gis.ru" ["geo", "search"] [("q", "Москва"), ("key", apiKey), ("version", "1.3")]
  -- print turl

get2GisApiKey :: IO (Maybe String)
get2GisApiKey = do
  (aurl, content) <- HN.getContentsByURL userAgent "https://2gis.ru"
  -- putStrLn (SU8.toString content)
  -- error ""
  let doc :: IOSArrow XmlTree XmlTree
      doc = readString
        [withParseHTML yes, withWarnings no, withInputEncoding utf8]
        (SU8.toString content)
  mapiKeyUrl <-
    getAPIScriptUrl
      .   DL.filter (DL.isSuffixOf "/app.js")
      <$> runX (doc >>> css "script" >>> getAttrValue "src" >>^ (aurl ++))
  case mapiKeyUrl of
    Just apikeyUrl -> do
      (_, apiKey) <-
        second
          ( maybeFromText
          . T.takeWhile (/= '"')
          . snd
          . T.breakOnEnd "n[\"WebApi.build.key\"]=\""
          . TE.decodeUtf8
          )
        <$> HN.getContentsByURL userAgent apikeyUrl
      return $ T.unpack <$> apiKey
    _ -> return Nothing

maybeFromText :: T.Text -> Maybe T.Text
maybeFromText t
  | T.length t > 0
  = Just t
  | otherwise
  = Nothing

getAPIScriptUrl :: [String] -> Maybe String
getAPIScriptUrl ss = case ss of
  (u:_) -> Just u
  _ -> Nothing

showGeoAO :: IO ()
showGeoAO =
  dbConn $ do
    as <- selectList ([] ::[Filter GeoAo]) []
    forM_ as $ \(Entity _ ao) -> do
      let geom :: GeoJsonPolygon
          Just (GeoJsonGeometryPolygon geom) =
            decode' . BL.fromStrict . geoAoJson $ ao
      liftIO
        $ forM_ (geoJsonPolygonObjects . geoInvertLinearRingPolygon $ geom)
        $ \lrs -> do
          TIO.putStrLn (geoAoName ao)
          TIO.putStrLn . TE.decodeUtf8 . BL.toStrict . encode $ lrs
          TIO.putStrLn "---"

showGeoMO :: IO ()
showGeoMO =
  dbConn $ do
    ms <- selectList ([] ::[Filter GeoMo]) []
    forM_ ms $ \(Entity _ mo) -> do
      let geom :: GeoJsonPolygon
          Just (GeoJsonGeometryPolygon geom) =
            decode' . BL.fromStrict . geoMoJson $ mo
      liftIO
        $ forM_ (geoJsonPolygonObjects . geoInvertLinearRingPolygon $ geom)
        $ \lrs -> do
          TIO.putStrLn (geoMoName mo <> " " <> geoMoNameAo mo)
          TIO.putStrLn . TE.decodeUtf8 . BL.toStrict . encode $ lrs
          TIO.putStrLn "---"

migrateGeoMOAO :: IO ()
migrateGeoMOAO = do
  Just geoAO <- fmap geoConvertMultiToPolygon . decode' <$> BL.readFile ("data" </> "ao-geo.json") :: IO (Maybe GeoJson)
  Just geoMO <- fmap geoConvertMultiToPolygon . decode' <$> BL.readFile ("data" </> "mo-geo.json") :: IO (Maybe GeoJson)
  runResourceT $ runNoLoggingT $ withMySQLConn rikardCorpDBci $ runSqlConn $ do
    runMigration migrateRikardCorpDB

    forM_ (geoJsonFeatures geoAO) $ \ao -> do
      let Just name = M.lookup "NAME" (geoJsonFeatureProperties ao)
          Just abbr = M.lookup "ABBREV" (geoJsonFeatureProperties ao)
          jsnc      = BL.toStrict . encode . geoJsonFeatureGeometry $ ao
      insert_ (GeoAo name abbr jsnc)

    forM_ (geoJsonFeatures geoMO) $ \mo -> do
      let Just name   = M.lookup "NAME" (geoJsonFeatureProperties mo)
          Just nameAo = M.lookup "NAME_AO" (geoJsonFeatureProperties mo)
          jsnc        = BL.toStrict . encode . geoJsonFeatureGeometry $ mo
      insert_ (GeoMo name nameAo jsnc)

  return ()

testJson1 :: T.Text
testJson1 = "{\"type\": \"FeatureCollection\", \"crs\": { \"type\": \"name\", \"properties\": { \"name\": \"urn:ogc:def:crs:OGC:1.3:CRS84\" }} }"

decodeJson1 :: Maybe GeoJson
decodeJson1 = decodeT testJson1

testJson2 :: T.Text
testJson2 = "{ \"type\": \"Polygon\", \"coordinates\": [ [ [100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0], [100.0, 0.0] ], [ [100.2, 0.2], [100.8, 0.2], [100.8, 0.8], [100.2, 0.8], [100.2, 0.2] ] ] }"

decodeJson2 :: Maybe GeoJsonGeometry
decodeJson2 = decodeT testJson2

testJson3 :: T.Text
testJson3 = "[100.0, 0.0]"

decodeJson3 :: Maybe GeoJsonPoint
decodeJson3 = decodeT testJson3

testJson4 :: T.Text
testJson4 = "[ [100.1, 2.0], [103.0, 4.0] ]"

decodeJson4 :: Maybe GeoJsonLinearRing
decodeJson4 = decodeT testJson4

testJson5 :: T.Text
testJson5 = "[ [ [100.1, 2.0], [103.0, 4.0] ], [ [100.1, 2.0], [103.0, 4.0] ] ]"

decodeJson5 :: Maybe GeoJsonPolygon
decodeJson5 = decodeT testJson5

testJson6 :: T.Text
testJson6 = "[ [ [ [100.1, 2.0], [103.0, 4.0] ], [ [100.1, 2.0], [103.0, 4.0] ] ], [ [ [100.1, 2.0], [103.0, 4.0] ], [ [100.1, 2.0], [103.0, 4.0] ] ] ]"

decodeJson6 :: Maybe GeoJsonMultiPolygon
decodeJson6 = decodeT testJson6

testJson7 :: T.Text
testJson7 = "{ \"type\": \"Feature\", \"properties\": { \"NAME\": \"Юго-Западный\", \"OKATO\": \"45293000\", \"ABBREV\": \"ЮЗАО\" }, \"geometry\": { \"type\": \"MultiPolygon\", \"coordinates\": [ [ [ [100.1, 2.0], [103.0, 4.0] ], [ [100.1, 2.0], [103.0, 4.0] ] ], [ [ [100.1, 2.0], [103.0, 4.0] ], [ [100.1, 2.0], [103.0, 4.0] ] ] ] } }"

decodeJson7 :: Maybe GeoJsonFeature
decodeJson7 = decodeT testJson7

-- testJson8 :: T.Text
-- testJson8 = "{\"type\": \"FeatureCollection\", \"crs\": { \"type\": \"name\", \"properties\": { \"name\": \"urn:ogc:def:crs:OGC:1.3:CRS84\" }}, \"features\": [ { \"type\": \"Feature\", \"properties\": { \"NAME\": \"Юго-Западный\", \"OKATO\": \"45293000\", \"ABBREV\": \"ЮЗАО\" }, \"geometry\": { \"type\": \"MultiPolygon\", \"coordinates\": [ [ [ [100.1, 2.0], [103.0, 4.0] ], [ [100.1, 2.0], [103.0, 4.0] ] ], [ [ [100.1, 2.0], [103.0, 4.0] ], [ [100.1, 2.0], [103.0, 4.0] ] ] ] } }, { \"type\": \"Feature\", \"properties\": { \"NAME\": \"Юго-Западный\", \"OKATO\": \"45293000\", \"ABBREV\": \"ЮЗАО\" }, \"geometry\": { \"type\": \"MultiPolygon\", \"coordinates\": [ [ [ [100.1, 2.0], [103.0, 4.0] ], [ [100.1, 2.0], [103.0, 4.0] ] ], [ [ [100.1, 2.0], [103.0, 4.0] ], [ [100.1, 2.0], [103.0, 4.0] ] ] ] } } ] }"
--
-- decodeJson8 :: Maybe GeoJson
-- decodeJson8 = decodeT testJson8

decodeE :: FromJSON a => T.Text -> Either String a
decodeE = eitherDecode' . BL.fromStrict . TE.encodeUtf8

decodeT :: FromJSON a => T.Text -> Maybe a
decodeT = decode' . BL.fromStrict . TE.encodeUtf8

dbConn
  :: (MonadIO m, MonadBaseControl IO m) =>
  SqlPersistT (NoLoggingT (ResourceT m)) a
  -> m a
dbConn =
  runResourceT . runNoLoggingT . withMySQLConn rikardCorpDBci . runSqlConn
