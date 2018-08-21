{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified BasicPrelude                  as BP
import           Control.Exception
import           Control.Monad                 as CM
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.UTF8          as SU8
import qualified Data.Char                     as C
import           Data.Conduit
import qualified Data.Conduit.List             as CL
import           Data.Either
import           Data.IORef
import qualified Data.List                     as DL
import qualified Data.Map.Strict               as MS
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Time.Clock
import           Data.XML.Types                (Event)
import           Database.Persist.MySQL
import           RikardCorp.DBConnection
import qualified RikardCorp.Helpers.DateTime   as HDT
import qualified RikardCorp.Helpers.Geo        as HG
import qualified RikardCorp.Helpers.IO         as HIO
import qualified RikardCorp.Helpers.Networking as HN
import qualified RikardCorp.Helpers.NLP.Text   as HNT
import qualified RikardCorp.Helpers.Text       as HT
import           RikardCorp.Types              hiding (tagName)
import qualified System.Directory              as SD
import           System.FilePath
import           Text.HandsomeSoup
import           Text.XML.HXT.Core
import           Text.XML.Stream.Parse

type StreetsMapCode = MS.Map T.Text AddrStreet
type Counter = IORef Int
type MissionDB = [([Naznachenie], [T.Text])]

data AddrPlace =
  AddrPlace
    (Maybe AddrHouseNum)
    (Maybe AddrBuildNum)
    (Maybe AddrStrucNum)
    deriving Show

newtype AddrHouseNum = AddrHouseNum T.Text deriving Show
newtype AddrBuildNum = AddrBuildNum T.Text deriving Show
newtype AddrStrucNum = AddrStrucNum T.Text deriving Show

data AdvertTypes =
      RentPremises
    | RentSellLand
    | SellPremises
    deriving Show

newtype LotsUrl (t :: AdvertTypes) =
  LotsUrl { unLotsUrl :: T.Text } deriving Show

newtype LotsFile (t :: AdvertTypes) =
  LotsFile { unLotsFile :: T.Text } deriving Show

newtype LotsDir (t :: AdvertTypes) =
  LotsDir { unLotsDir :: T.Text } deriving Show

data Notification (t :: AdvertTypes) = Notification
  { notificationBidKindId :: Maybe T.Text
  , notificationBidKindName :: Maybe T.Text
  , notificationBidNumber :: Maybe T.Text
  , notificationOrganizationName :: Maybe T.Text
  , notificationIsArchived :: Maybe T.Text
  , notificationPublishDate :: Maybe T.Text
  , notificationLastChanged :: Maybe T.Text
  , notificationOdDetailedHref :: Maybe T.Text
  } deriving Show

data NotificationFull = NotificationFull
  { notificationFullDetails :: NDetails
  , notificationFullProtocols :: [NProtocol]
  } deriving Show

data NProtocol = NProtocol
  { nProtocolType :: Maybe T.Text
  , nProtocolNum :: Maybe T.Text
  , nProtocolTown :: Maybe T.Text
  , nProtocolDate :: Maybe T.Text
  , nProtocolPlace :: Maybe T.Text
  , nProtocolCommittee :: Maybe NProtocolCommittee
  , nProtocolLots :: [NProtocolLot]
  } deriving Show

data NProtocolLot = NProtocolLot
  { nProtocolLotLotNum :: Maybe T.Text
  , nProtocolLotCancelReason :: Maybe T.Text
  , nProtocolLotBidMember :: [NProtocolLotBidMember]
  , nProtocolLotDocuments :: Maybe [NProtocolLotDocument]
  , nProtocolLotDecision :: Maybe T.Text
  } deriving Show

data NProtocolLotDocument = NProtocolLotDocument
  { nProtocolLotDocumentDocType :: Maybe T.Text
  , nProtocolLotDocumentDescription :: Maybe T.Text
  , nProtocolLotDocumentCreated :: Maybe T.Text
  , nProtocolLotDocumentDocUrl :: Maybe T.Text
  } deriving Show

data NProtocolLotBidMember = NProtocolLotBidMember
  { nProtocolLotBidMemberRegNum :: Maybe T.Text
  , nProtocolLotBidMemberName :: Maybe T.Text
  , nProtocolLotBidMemberInn :: Maybe T.Text
  , nProtocolLotBidMemberKpp :: Maybe T.Text
  , nProtocolLotBidMemberOgrn :: Maybe T.Text
  , nProtocolLotBidMemberLocation :: Maybe T.Text
  , nProtocolLotBidMemberPhone :: Maybe T.Text
  , nProtocolLotBidMemberIsRemoved :: Maybe T.Text
  , nProtocolLotBidMemberTenderPosition :: Maybe T.Text
  , nProtocolLotBidMemberFinalScore :: Maybe T.Text
  , nProtocolLotBidMemberDocuments :: Maybe T.Text
  , nProtocolLotBidMemberAttended :: Maybe T.Text
  , nProtocolLotBidMemberOffer :: Maybe T.Text
  , nProtocolLotBidMemberPricePerAreaSm :: Maybe T.Text
  , nProtocolLotBidMemberIsSelected :: Maybe T.Text
  , nProtocolLotBidMemberRefuseReason :: Maybe T.Text
  , nProtocolLotBidMemberRejectReason :: Maybe T.Text
  } deriving Show

data NProtocolCommittee = NProtocolCommittee
  { nProtocolCommitteeMembers :: [NProtocolCommitteeMember]
  , nProtocolCommitteeCommittePercent :: Maybe T.Text
  } deriving Show

data NProtocolCommitteeMember = NProtocolCommitteeMember
  { nProtocolCommitteeMemberName :: Maybe T.Text
  , nProtocolCommitteeMemberRole :: Maybe T.Text
  } deriving Show

data NDetails = NDetails
  { nDetailsBidNumber :: Maybe T.Text
  , nDetailsBidOrganization :: Maybe NDetailsBidOrganization
  , nDetailsCommon :: Maybe NDetailsCommon
  , nDetailsDocuments :: Maybe [NDetailsDocument]
  , nDetailsLots :: [NDetailsLot]
  } deriving Show

data NDetailsLot = NDetailsLot
  { nDetailsLotLotNum :: Maybe T.Text
  , nDetailsLotId :: Maybe T.Text -- RentSellLand
  , nDetailsLotBidStatus :: Maybe NDetailsLotBidStatus
  , nDetailsLotsuspendDate :: Maybe T.Text
  , nDetailsLotsuspendReason :: Maybe T.Text
  , nDetailsLotCancelDate :: Maybe T.Text
  , nDetailsLotCancelReason :: Maybe T.Text
  , nDetailsLotBidType :: Maybe NDetailsLotBidType -- RentSellLand
  , nDetailsLotPropertyType :: Maybe NDetailsLotPropertyType
  , nDetailsLotPropKind :: Maybe NDetailsLotPropKind
  , nDetailsLotReqDecision :: Maybe T.Text -- RentSellLand
  , nDetailsLotCadastralNum :: Maybe T.Text -- RentSellLand
  , nDetailsLotTorgReason :: Maybe T.Text -- SellPremises
  , nDetailsLotOrgFullName :: Maybe T.Text -- SellPremises
  , nDetailsLotPropDesc :: Maybe T.Text -- SellPremises
  , nDetailsLotObjectCode :: Maybe T.Text
  , nDetailsLotDescription :: Maybe T.Text
  , nDetailsLotMission :: Maybe T.Text
  , nDetailsLotKladrLocation :: Maybe NDetailsLotKladrLocation
  , nDetailsLotPostAddress :: Maybe T.Text -- SellPremises
  , nDetailsLotFundSize :: Maybe T.Text -- SellPremises
  , nDetailsLotOrgNominalValue :: Maybe T.Text -- SellPremises
  , nDetailsLotAcsPart :: Maybe T.Text -- SellPremises
  , nDetailsLotLocation :: Maybe T.Text
  , nDetailsLotUnit :: Maybe NDetailsLotUnit -- RentSellLand
  , nDetailsLotArea :: Maybe T.Text -- RentSellLand
  , nDetailsLotDescription2 :: Maybe T.Text -- RentSellLand
  , nDetailsLotStartSalePrice :: Maybe T.Text -- SellPremises
  , nDetailsLotCondition :: Maybe T.Text -- SellPremises
  , nDetailsLotFederalStockPercent :: Maybe T.Text -- SellPremises
  , nDetailsLotStockNum :: Maybe T.Text -- SellPremises
  , nDetailsLotStockPercentSale :: Maybe T.Text -- SellPremises
  , nDetailsLotMinPrice :: Maybe T.Text -- SellPremises
  , nDetailsLotPriceStep :: Maybe T.Text -- SellPremises
  , nDetailsLotStepNegative :: Maybe T.Text -- SellPremises
  , nDetailsLotWorkList :: Maybe T.Text -- SellPremises
  , nDetailsLotMarketPartDesc :: Maybe T.Text -- SellPremises
  , nDetailsLotAreaUnmovable :: Maybe T.Text -- SellPremises
  , nDetailsLotObjectsList :: Maybe T.Text -- SellPremises
  , nDetailsLotEmplNum :: Maybe T.Text -- SellPremises
  , nDetailsLotDocsList :: Maybe T.Text -- SellPremises
  , nDetailsLotAreaMeters :: Maybe T.Text
  , nDetailsLotTermYear :: Maybe T.Text
  , nDetailsLotTermMonth :: Maybe T.Text
  , nDetailsLotTermDay :: Maybe T.Text
  , nDetailsLotArticle :: Maybe NDetailsLotArticle
  , nDetailsLotPricePerMonth :: Maybe T.Text -- RentSellLand
  , nDetailsLotPricePerYear :: Maybe T.Text -- RentSellLand
  , nDetailsLotStartPrice :: Maybe T.Text -- RentSellLand
  , nDetailsLotStartPriceAreaSm :: Maybe T.Text -- RentSellLand
  , nDetailsLotStep :: Maybe T.Text -- RentSellLand
  , nDetailsLotMonthPrice :: Maybe T.Text
  , nDetailsLotYearPrice :: Maybe T.Text
  , nDetailsLotDealFee :: Maybe T.Text
  , nDetailsLotContractFee :: Maybe T.Text
  , nDetailsLotStartPricePerMonth :: Maybe T.Text
  , nDetailsLotIsOverLimitDeal :: Maybe T.Text
  , nDetailsLotDepositSize :: Maybe T.Text
  , nDetailsLotDepositDesc2 :: Maybe T.Text -- RentSellLand
  , nDetailsLotMaintenanceSize :: Maybe T.Text
  , nDetailsLotBuildConditions :: Maybe T.Text -- RentSellLand
  , nDetailsLotTechConditions :: Maybe T.Text -- RentSellLand
  , nDetailsLotIsBurdened :: Maybe T.Text
  , nDetailsLotBurdenDescription :: Maybe T.Text
  , nDetailsLotDepositSize2 :: Maybe T.Text -- SellPremises
  , nDetailsLotDepositDesc :: Maybe T.Text -- SellPremises
  , nDetailsLotContractDesc :: Maybe T.Text -- SellPremises
  , nDetailsLotLimit :: Maybe T.Text -- SellPremises
  , nDetailsLotWinnerDefineDesc :: Maybe T.Text -- SellPremises
  , nDetailsLotPrivateConditions :: Maybe T.Text -- SellPremises
  , nDetailsLotLastInfo :: Maybe T.Text -- SellPremises
  , nDetailsLotSinglePrice :: Maybe T.Text -- SellPremises
  , nDetailsLotFinalPrice :: Maybe T.Text -- SellPremises
  , nDetailsLotResult :: Maybe T.Text -- SellPremises
  , nDetailsLotIsSubrent :: Maybe T.Text
  , nDetailsLotLotPhotosExist :: Maybe T.Text
  , nDetailsLotGroundViewPlace :: Maybe T.Text -- RentSellLand
  , nDetailsLotArticleVal :: Maybe T.Text -- RentSellLand
  , nDetailsLotResultStartPriceAreaSm :: Maybe T.Text -- RentSellLand
  , nDetailsLotBidResults :: Maybe T.Text -- RentSellLand
  , nDetailsLotContractNum :: Maybe T.Text
  , nDetailsLotContractDate :: Maybe T.Text
  , nDetailsLotContractPayment :: Maybe T.Text
  , nDetailsLotContractPriceYear :: Maybe T.Text
  , nDetailsLotContractPriceMonth :: Maybe T.Text
  , nDetailsLotContractPriceHour :: Maybe T.Text
  , nDetailsLotWinner :: Maybe NDetailsLotWinner
  , nDetailsLotCurrency :: Maybe T.Text
  , nDetailsLotCurrencyPercent :: Maybe T.Text
  , nDetailsLotPaymentRequisites :: Maybe NDetailsLotPaymentRequisites
  , nDetailsLotDocuments :: Maybe [NDetailsLotDocument]
  , nDetailsLotResults :: Maybe [NDetailsLotResultsMember] -- SellPremises
  } deriving Show

data NDetailsLotResultsMember = NDetailsLotResultsMember
  { nDetailsLotResultsMemberRegNum :: Maybe T.Text
  , nDetailsLotResultsMemberName :: Maybe T.Text
  , nDetailsLotResultsMemberInn :: Maybe T.Text
  , nDetailsLotResultsMemberKpp :: Maybe T.Text
  , nDetailsLotResultsMemberOgrnip :: Maybe T.Text
  , nDetailsLotResultsMemberOgrn :: Maybe T.Text
  , nDetailsLotResultsMemberLocation :: Maybe T.Text
  , nDetailsLotResultsMemberPhone :: Maybe T.Text
  , nDetailsLotResultsMemberIsRemoved :: Maybe T.Text
  , nDetailsLotResultsMemberIsSelected :: Maybe T.Text
  , nDetailsLotResultsMemberRefuseReason :: Maybe T.Text
  , nDetailsLotResultsMemberTenderPosition :: Maybe T.Text
  , nDetailsLotResultsMemberContractTearms :: Maybe T.Text
  , nDetailsLotResultsMemberOffer :: Maybe T.Text
  } deriving Show

data NDetailsLotDocument = NDetailsLotDocument
  { nDetailsLotDocumentDocType :: Maybe T.Text
  , nDetailsLotDocumentDocDate :: Maybe T.Text
  , nDetailsLotDocumentDocNum :: Maybe T.Text
  , nDetailsLotDocumentDescription :: Maybe T.Text
  , nDetailsLotDocumentCreated :: Maybe T.Text
  , nDetailsLotDocumentDocUrl :: Maybe T.Text
  } deriving Show

data NDetailsLotPaymentRequisites = NDetailsLotPaymentRequisites
  { nDetailsLotPaymentRequisitesBik :: Maybe T.Text
  , nDetailsLotPaymentRequisitesBankName :: Maybe T.Text
  , nDetailsLotPaymentRequisitesKs :: Maybe T.Text
  , nDetailsLotPaymentRequisitesRs :: Maybe T.Text
  , nDetailsLotPaymentRequisitesPs :: Maybe T.Text
  } deriving Show

data NDetailsLotWinner = NDetailsLotWinner
  { nDetailsLotWinnerName :: Maybe T.Text
  , nDetailsLotWinnerInn :: Maybe T.Text
  , nDetailsLotWinnerKpp :: Maybe T.Text
  , nDetailsLotWinnerOgrnip :: Maybe T.Text
  , nDetailsLotWinnerOgrn :: Maybe T.Text
  , nDetailsLotWinnerLocation :: Maybe T.Text
  , nDetailsLotWinnerPhone :: Maybe T.Text
  } deriving Show

data NDetailsLotArticle = NDetailsLotArticle
  { nDetailsLotArticleId :: Maybe T.Text
  , nDetailsLotArticleName :: Maybe T.Text
  } deriving Show

data NDetailsLotKladrLocation = NDetailsLotKladrLocation
  { nDetailsLotKladrLocationId :: Maybe T.Text
  , nDetailsLotKladrLocationName :: Maybe T.Text
  } deriving Show

data NDetailsLotPropKind = NDetailsLotPropKind
  { nDetailsLotPropKindId :: Maybe T.Text
  , nDetailsLotPropKindName :: Maybe T.Text
  } deriving Show

data NDetailsLotPropertyType = NDetailsLotPropertyType
  { nDetailsLotPropertyTypeId :: Maybe T.Text
  , nDetailsLotPropertyTypeName :: Maybe T.Text
  } deriving Show

data NDetailsLotUnit = NDetailsLotUnit
  { nDetailsLotUnitId :: Maybe T.Text
  , nDetailsLotUnitName :: Maybe T.Text
  } deriving Show

data NDetailsLotBidType = NDetailsLotBidType
  { nDetailsLotBidTypeId :: Maybe T.Text
  , nDetailsLotBidTypeName :: Maybe T.Text
  } deriving Show

data NDetailsLotBidStatus = NDetailsLotBidStatus
  { nDetailsLotBidStatusId :: Maybe T.Text
  , nDetailsLotBidStatusName :: Maybe T.Text
  } deriving Show

data NDetailsDocument = NDetailsDocument
  { nDetailsDocumentDocType :: Maybe T.Text
  , nDetailsDocumentDocDate :: Maybe T.Text
  , nDetailsDocumentDescription :: Maybe T.Text
  , nDetailsDocumentCreated :: Maybe T.Text
  , nDetailsDocumentDocUrl :: Maybe T.Text
  } deriving Show

data NDetailsCommon = NDetailsCommon
  { nDetailsCommonBidKind :: Maybe NDetailsCommonBidKind
  , nDetailsCommonBidForm :: Maybe NDetailsCommonBidForm
  , nDetailsCommonBidUrl :: Maybe T.Text
  , nDetailsCommonFio :: Maybe T.Text
  , nDetailsCommonLotNum :: Maybe T.Text
  , nDetailsCommonPublished :: Maybe T.Text
  , nDetailsCommonLastChanged :: Maybe T.Text
  , nDetailsCommonTimeOut :: Maybe T.Text
  , nDetailsCommonNotificationUrl :: Maybe T.Text
  , nDetailsCommonIsFas :: Maybe T.Text -- SellPremises
  , nDetailsCommonStartDateRequest :: Maybe T.Text -- SellPremises
  , nDetailsCommonIsOnlySmps :: Maybe T.Text
  , nDetailsCommonDocProvide :: Maybe T.Text
  , nDetailsCommonDocChargeRateRur :: Maybe T.Text
  , nDetailsCommonExpireDate :: Maybe T.Text
  , nDetailsCommonAppReceiptDetails :: Maybe T.Text -- RentSellLand
  , nDetailsCommonAppRequirement :: Maybe T.Text -- RentSellLand
  , nDetailsCommonAppWithdraw :: Maybe T.Text -- RentSellLand
  , nDetailsCommonCondition :: Maybe T.Text -- RentSellLand
  , nDetailsCommonWinnerDefineDate :: Maybe T.Text -- RentSellLand
  , nDetailsCommonWinnerDefinePlace :: Maybe T.Text -- RentSellLand
  , nDetailsCommonAppChange :: Maybe T.Text -- RentSellLand
  , nDetailsCommonPlaceRequest :: Maybe T.Text -- SellPremises
  , nDetailsCommonBidAuctionDate :: Maybe T.Text
  , nDetailsCommonBidAuctionPlace :: Maybe T.Text
  , nDetailsCommonSummationDate2 :: Maybe T.Text -- RentSellLand
  , nDetailsCommonSummationPlace :: Maybe T.Text -- SellPremises
  , nDetailsCommonWinnerDefineDescr :: Maybe T.Text -- RentSellLand
  , nDetailsCommonBulletinNumber :: Maybe T.Text -- SellPremises
  , nDetailsCommonProcessingDate :: Maybe T.Text
  , nDetailsCommonSummationDate :: Maybe T.Text
  } deriving Show

data NDetailsCommonBidForm = NDetailsCommonBidForm
  { nDetailsCommonBidFormId :: Maybe T.Text
  , nDetailsCommonBidFormName :: Maybe T.Text
  } deriving Show

data NDetailsCommonBidKind = NDetailsCommonBidKind
  { nDetailsCommonBidKindId :: Maybe T.Text
  , nDetailsCommonBidKindName :: Maybe T.Text
  } deriving Show

data NDetailsBidOrganization = NDetailsBidOrganization
  { nDetailsBidOrganizationBidOrgKind :: Maybe T.Text
  , nDetailsBidOrganizationOrganizationId :: Maybe T.Text
  , nDetailsBidOrganizationFullName :: Maybe T.Text
  , nDetailsBidOrganizationHeadOrg :: Maybe T.Text
  , nDetailsBidOrganizationLimitBidDeal :: Maybe T.Text
  , nDetailsBidOrganizationInn :: Maybe T.Text
  , nDetailsBidOrganizationKpp :: Maybe T.Text
  , nDetailsBidOrganizationOkato :: Maybe T.Text
  , nDetailsBidOrganizationOkpo :: Maybe T.Text
  , nDetailsBidOrganizationOkved :: Maybe T.Text
  , nDetailsBidOrganizationOgrn :: Maybe T.Text
  , nDetailsBidOrganizationAddress :: Maybe T.Text
  , nDetailsBidOrganizationPhone :: Maybe T.Text
  , nDetailsBidOrganizationFax :: Maybe T.Text
  , nDetailsBidOrganizationLocation :: Maybe T.Text
  , nDetailsBidOrganizationUrl :: Maybe T.Text
  } deriving Show

userAgent :: String
userAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:48.0) Gecko/20100101 Firefox/48.0"

-- RentPremises
rentPremisesLotsUrl :: LotsUrl 'RentPremises
rentPremisesLotsUrl = LotsUrl "http://torgi.gov.ru/opendata/7710349494-torgi/data-1-{DATE_FROM}-{DATE_TO}-structure-20130401T0000.xml"

rentPremisesLotsFile :: LotsFile 'RentPremises
rentPremisesLotsFile =
  LotsFile . T.pack $ ".." </> "rikardru-corp-dbdata" </> (show RentPremises <> ".xml")

rentPremisesLotsDir :: LotsDir 'RentPremises
rentPremisesLotsDir =
  LotsDir . T.pack $ ".." </> "rikardru-corp-dbdata" </> show RentPremises

-- SellPremises
sellPremisesLotsUrl :: LotsUrl 'SellPremises
sellPremisesLotsUrl = LotsUrl "http://torgi.gov.ru/opendata/7710349494-torgi/data-8-{DATE_FROM}-{DATE_TO}-structure-20130401T0000.xml"

sellPremisesLotsFile :: LotsFile 'SellPremises
sellPremisesLotsFile =
  LotsFile . T.pack $ ".." </> "rikardru-corp-dbdata" </> (show SellPremises <> ".xml")

sellPremisesLotsDir :: LotsDir 'SellPremises
sellPremisesLotsDir =
  LotsDir . T.pack $ ".." </> "rikardru-corp-dbdata" </> show SellPremises

-- RentSellLand
rentSellLandLotsUrl :: LotsUrl 'RentSellLand
rentSellLandLotsUrl = LotsUrl "http://torgi.gov.ru/opendata/7710349494-torgi/data-2-{DATE_FROM}-{DATE_TO}-structure-20130401T0000.xml"

rentSellLandLotsFile :: LotsFile 'RentSellLand
rentSellLandLotsFile =
  LotsFile . T.pack $ ".." </> "rikardru-corp-dbdata" </> (show RentSellLand <> ".xml")

rentSellLandLotsDir :: LotsDir 'RentSellLand
rentSellLandLotsDir =
  LotsDir . T.pack $ ".." </> "rikardru-corp-dbdata" </> show RentSellLand

fillLotsUrlDates :: T.Text -> T.Text -> LotsUrl t -> LotsUrl t
fillLotsUrlDates dfrom dto (LotsUrl u) =
  LotsUrl (T.replace "{DATE_TO}" dto . T.replace "{DATE_FROM}" dfrom $ u)

main :: IO ()
main = do
  dbConn $ runMigration migrateRikardCorpDB
  downloadNs rentPremisesLotsUrl rentPremisesLotsFile rentPremisesLotsDir
  -- downloadNs sellPremisesLotsUrl sellPremisesLotsFile sellPremisesLotsDir
  -- downloadNs rentSellLandLotsUrl rentSellLandLotsFile rentSellLandLotsDir
  -- addAdvertsRentPremises rentPremisesLotsFile rentPremisesLotsDir
  -- ao <- getGeoAOList
  -- mo <- getGeoMOList
  -- let
  --   pt = HG.pt
  --         (read @Double "55.752024")
  --         (read @Double "37.619363")
  --         Nothing
  --         Nothing
  -- print $ HG.getGeoAO ao okrugTextMap pt
  -- print $ HG.getGeoMO mo rayonTextMap pt
  -- return ()

getGeoAOList :: IO [GeoAo]
getGeoAOList =
  fmap entityVal <$> dbConn (selectList ([] :: [Filter GeoAo]) [])

getGeoMOList :: IO [GeoMo]
getGeoMOList =
  fmap entityVal <$> dbConn (selectList ([] :: [Filter GeoMo]) [])

addAdvertsRentPremises
  :: LotsFile 'RentPremises -> LotsDir 'RentPremises -> IO ()
addAdvertsRentPremises lf ld = do
  let fpDir = T.unpack . unLotsDir $ ld
  dirExists <- SD.doesDirectoryExist fpDir
  unless dirExists . error $ fpDir <> " doesn`t exists."
  (aucChkTime, _) <- HDT.subtractDaysFromCurrent 30
  streets <-  MS.fromList
          .   fmap ((addrStreetCode . entityVal) &&& entityVal)
          <$> dbConn (selectList ([] :: [Filter AddrStreet]) [])
  gCount <- newIORef @Int 0
  bCount <- newIORef @Int 0
  runResourceT
     $ parseFile def (T.unpack $ unLotsFile lf)
    $$ parseOpenData
    =$ CL.mapM (lift . getNotificationFull ld)
    =$ CL.filter (isJust . snd)
    =$ CL.filter (isJust
                . nDetailsCommon
                . notificationFullDetails
                . fromJust
                . snd)
    =$ CL.filter (filterRentPremisesMoscow . snd)
    =$ CL.filter (filterRentPremisesProperty . snd)
    -- =$ CL.filter (filterRentPremisesFreshAuc aucChkTime
    --             . nDetailsCommon
    --             . notificationFullDetails
    --             . fromJust
    --             . snd)
    =$ CL.mapM_ (lift . addRentPremisesNotification gCount bCount streets ld)
  gc <- readIORef gCount
  bc <- readIORef bCount
  print
    $ "Good: " <> show gc
   <> ", Bad: " <> show bc
   <> ", Total: " <> show (gc + bc)

addRentPremisesNotification
  :: Counter
  -> Counter
  -> StreetsMapCode
  -> LotsDir 'RentPremises
  -> (FilePath, Maybe NotificationFull)
  -> IO ()
addRentPremisesNotification gc bc smc _ld (np, nFull') = do
  let nFull = fromJust nFull'
  let Just nm = nDetailsBidNumber . notificationFullDetails $ nFull
  putStrLn np
  TIO.putStrLn nm
  printLocations gc bc smc . nDetailsLots . notificationFullDetails $ nFull
  -- printDocuments . nDetailsDocuments . notificationFullDetails $ nFull
  -- print nFull
  -- _ <- error "..."
  putStrLn "################################"
  return ()

printLocations
  :: Counter -> Counter -> StreetsMapCode -> [NDetailsLot] -> IO ()
printLocations gc bc smc = mapM_ printLocation
  where
    printLocation :: NDetailsLot -> IO ()
    printLocation lot = do
      let location = nDetailsLotLocation lot
          description = nDetailsLotDescription lot
          mission = nDetailsLotMission lot
          addrPlace' = parseAddPlace location
          addrPlace =
            if isAddrPlaceEmpty addrPlace'
              then parseAddPlace description
              else addrPlace'
          kladrLocation = nDetailsLotKladrLocation lot
          (Just kladrCode) =
            nDetailsLotKladrLocationId . fromJust $ kladrLocation
      CM.when ("77" `T.isPrefixOf` kladrCode) $ do
        maybe (return ()) (TIO.putStrLn . (<>) "L: ") location
        maybe (return ()) (TIO.putStrLn . (<>) "D: ") description
        TIO.putStrLn . (<>) "M: " $ fromMaybe "Nothing" mission
        TIO.putStrLn . (<>) "MS: " . BP.tshow $ getMissions mission
        CM.when (isAddrPlaceEmpty addrPlace) $ do
          modifyIORef' bc (+1)
          TIO.putStrLn "Addr empty"
        CM.unless (isAddrPlaceEmpty addrPlace) $ do
          TIO.putStrLn $ "Addr: " <> BP.tshow addrPlace
          addrStreet <-
              maybe
                (extractStreetAddr location description)
                (return . Just)
                $ MS.lookup kladrCode smc
          CM.when (isJust addrStreet) $ do
            TIO.putStrLn
              . fromMaybe "NONAME"
              . nDetailsLotKladrLocationName
              . fromJust
              $ kladrLocation
            TIO.putStrLn kladrCode
            let (Just street) = addrStreet
            house <- getHouseByAddr street addrPlace
            case house of
              Just aHouse -> do
                modifyIORef' gc (+1)
                putStrLn $ "Centroid: " <> show (addrHouseCentroid aHouse)
                TIO.putStrLn $ "Floor: " <> showFloor location description
                putStrLn $ "Rooms: " <> show (getRoomsCountDL location description)
              _ -> do
                modifyIORef' bc (+1)
                putStrLn "No house"
          CM.when (isNothing addrStreet) $ do
            modifyIORef' bc (+1)
            TIO.putStrLn $ "Error: Kladr is absent in DB: " <> kladrCode
            TIO.putStrLn
              . fromMaybe "NONAME"
              . nDetailsLotKladrLocationName
              . fromJust
              $ kladrLocation

textToMissionDB :: MissionDB
textToMissionDB =
  [
    (
    [MEDICINA],
    [ "оказание медицинских услуг"
    , "медицинское"
    , "детская офтальмология"
    , "под медицинскую деятельность"
    , "под медицинские услуги"
    , "для использования под размещение стоматологической клиники"
    , "объект здравоохранения"
    , "ветеринарная клиника"
    ]
  )
  , (
    [OFIS],
    [ "офис"
    , "офисное"
    , "учрежденческие цели"
    , "учрежденческие"
    , "под офис"
    , "под административные цели"
    , "административное"
    , "акдминистративное"
    , "офисное помещение"
    , "для использования под офис"
    , "размещение офиса"
    , "административно-офисное"
    , "офис класса с"
    , "под конторские помещения"
    , "под размещение офиса"
    , "этажное офисное помещение"
    , "офисные помещения в строении на территории домовладения"
    , "административное использование"
    , "офисные помещения"
    , "для использование под офисные помещения"
    , "нежилое помещение для размещения офиса"
    , "административное и (или) хозяйственное"
    , "под служебные цели"
    , "офисная"
    , "целевое назначение административное (офис)"
    , "целевое назначение административное"
    , "офис-продаж"
    , "учрежденческое"
    , "административное помещение"
    , "под офис медицинской организации"
    , "цель аренды под офис"
    ]
  )
  , (
    [SVOBODNOE_NAZNACHENIE],
    [ "свободное"
    , "помещения свободного назначения"
    , "помещение свободного назначения"
    , "функциональное назначение свободное"
    , "свободное назначение"
    , "нежилое"
    , "назначение нежилое"
    , "помещения должны использоваться в качестве нежилого"
    , "свободного назначения"
    , "нежилые помещения"
    , "нежилое помещение"
    , "свободное в соответствии с действующим законодательством рф"
    , "свободное (исключения изложены в документации)"
    , "нежилые здания"
    , "нежиое помещение"
    , "нежилые помещения свободного назначения"
    ]
  )
  , (
    [TORGOVAJA_PLOSHHAD, OFIS],
    ["торгово-офисное"]
  )
  , (
    [BYTOVYE_USLUGI],
    [ "бытовое обслуживание"
    , "социальные услуги"
    , "оказание услуг населению"
    , "парикмахерская"
    , "специализация объекта-бытовые услуги"
    , "бытовые услуги"
    , "предоставление бытовых услуг населению"
    , "услуги"
    ]
  )
  , (
    [SKLAD],
    [ "склад"
    , "складское"
    , "складские"
    , "под склад"
    , "размещение склада"
    , "помещение под склад"
    , "под размещение склада"
    , "складское помещение в жилом доме"
    , "теплый склад"
    , "склад (неотапливаемый)"
    , "холодный склад"
    , "складские помещения"
    , "складское помещение"
    , "техническое помещение (склад)"
    , "складская"
    , "под cклад"
    ]
  )
  , (
    [DOU, OBRAZOVATELNOE],
    [ "объект негосударственного дошкольного образования"
    , "дошкольное образование"
    ]
  )
  , (
    [OBRAZOVATELNOE],
    [ "функциональное назначение начальное общее"
    , "начальное общее"
    , "осуществление образовательной деятельности"
    ]
  )
  , (
    [AVTOSTOJANKA],
    [ "автостоянка"
    , "под размещение автотранспорта"
    , "для стоянки автотранспорта"
    , "под стоянку автотранспортного средства"
    , "под стоянку автотранспорта"
    , "размещение стоянки автотранспорта"
    ]
  )
  , (
    [AVTOSERVIS],
    [ "пункт технического обслуживания"
    , "автосервис"
    ]
  )
  , (
    [AVTOMOJKA],
    ["автомойка"]
  )
  , (
    [GARAZH],
    ["гараж"]
  )
  , (
    [PROIZVODSTVO],
    [ "легкое производство"
    , "помещения пригодны для использования в производственных целях"
    , "производство"
    , "производственное"
    , "помещение производственного назначения"
    , "размещение производства"
    , "под производство"
    , "производственное использование"
    , "производственные помещения"
    , "научно-производственные помещения"
    , "производ помещение"
    , "производственное помещение"
    , "помещение приветственного назначения"
    , "производственная"
    , "под пищевое производство"
    , "произволство"
    , "пр-во"
    ]
  )
  , (
    [FOK],
    [ "физкультурно-оздоровительный комплекс"
    , "объект предназначен для оказания услуг тренажерного зала"
    ]
  )
  , (
    [OBSHHEPIT,STOLOVAJA],
    ["организация питания"]
  )
  , (
    [OBSHHEPIT, KAFE],
    [ "буфет"
    , "кафе"
    , "организация общественного питания (буфет)"
    ]
  )
  , (
    [OBSHHEPIT],
    [ "организация общественного питания"
    , "общественное питание"
    , "предприятие общественного питания"
    , "организация пункта общественного питания"
    , "нежилое помещение под пункт общественного питания"
    , "общепит"
    , "общественное питание (в соответствии с документацией об аукционе)"
    , "общественное питание-в соответствии с документацией об аукционе"
    , "пункт общественного питания (ресторан быстрого питания)"
    ]
  )
  , (
    [STOLOVAJA],
    [ "под пищевое производство (столовая)"
    , "столовая"
    , "кухня для приготовления пищи для сотрудников института и посетителей"
    , "организация питания для сотрудников и посетителей больницы"
    , "под организацию питания сотрудников арендодателя"
    ]
  )
  , (
    [RESTORAN],
    ["ресторан"]
  )
  , (
    [MAGAZIN],
    [ "магазин"
    , "продовольственный магазин с отделом кулинария"
    , "книжный магазин"
    , "канцелярский магазин"
    ]
  )
  , (
    [MAGAZIN,TORGOVAJA_PLOSHHAD],
    [ "реализация продуктов питания"
    , "размещение магазина по продаже продовольственных товаров"
    ]
  )
  , (
    [TORGOVAJA_PLOSHHAD],
    [ "торговое помещение"
    , "предприятие розничной торговли"
    , "специализация объекта-галантерейные товары"
    , "специализация объекта-печать"
    , "розничная торговля"
    , "галантерейные товары"
    , "церковная продукция"
    , "продовольственные товары"
    , "торговля изделиями оптики"
    , "торговое"
    , "торговый центр"
    , "торговое (неотапливаемое)"
    , "для организации торговли продуктами питания и сопутствующими товарами"
    , "под торговлю"
    , "хозяйственные товары"
    , "объект предназначен для торговли товарами спортивного назначения"
    , "под торговый павильон"
    , "торговля"
    , "торговля кондитерскими изделиями"
    , "торговля книгами"
    , "розничная торговля алкогольной продукцией"
    , "торговая точка"
    ]
  )
  , (
    [OFIS, SKLAD],
    [ "офисное-складское"
    , "склад и офис"
    , "офис-склад"
    ]
  )
  , (
    [KVARTIRA],
    [ "для проживания сотрудников арендатора (штатных-внештатных) и членов их семей"
    , "для размещения сотрудников арендатора (штатных-внештатных)"
    ]
  )
  , (
    [MASHINOMESTO],
    [ "машиноместо"
    , "машиноместа"
    , "машиноместа на подземной охраняемой парковке"
    , "машиноместо на подземной охраняемой парковке"
    , "машиномест в многоуровневом отапливаемом гараже"
    , "машиноместо на уровне подземной охраняемой парковки"
    ]
  )
  , (
    [PROIZVODSTVO, SKLAD],
    [ "в производственно-складских целях"
    , "производственно-складское использование"
    , "производственно-складские"
    , "производственно-складское"
    , "производственно-складская"
    , "под производственно-складское помещение"
    , "производственно-складское с административными помещениями"
    , "производственно-складские помещения"
    , "производственно-складское класса с"
    , "под производственные помещения для приготовления пищи"
    , "под производственно-складские помещения"
    , "цель аренды под производственно-складские помещения"
    ]
  )
  , (
    [KAFE, STOLOVAJA],
    ["кафе-столовая"]
  )
  , (
    [TORGOVAJA_PLOSHHAD, BYTOVYE_USLUGI, OFIS],
    ["торгово-офисное-быт обслуживание"]
  )
  , (
    [APTEKA],
    ["аптека"]
  )
  , (
    [GOSTINICA],
    ["общежитие"]
  )
  , (
    [OFIS, PROIZVODSTVO],
    ["офисно-производственное"]
  )
  ]

getMissions :: Maybe T.Text -> [Naznachenie]
getMissions Nothing = []
getMissions (Just text) =
    DL.nub
  . concat
  . fmap (DL.concatMap (textToMission textToMissionDB) . fixText)
  . T.lines
  $ text
  where
    textToMission :: MissionDB -> T.Text -> [Naznachenie]
    textToMission [] _ = []
    textToMission ((ns, nss):ms) miss =
      if DL.any ((==) miss) nss
        then ns
        else textToMission ms miss

    fixText :: T.Text -> [T.Text]
    fixText text' =
        HT.cleanText'
      . T.split (==',')
      . T.toLower
      . HT.cleanExcessSpaces
      . T.map (\ch ->
        if C.isLetter ch
          || ch == '-'
          || ch == ','
          || ch == ')'
          || ch == '('
          then ch
          else ' ')
      . T.map (\ch ->
        if ch == '/'
          || ch == ';'
          then ','
          else ch)
      . T.replace "+" "-"
      . T.replace "(штатных/внештатных)" "(штатных-внештатных)"
      . T.replace " - " "-"
      . T.replace "- " "-"
      . T.replace " -" "-"
      $ text'


showFloor :: Maybe T.Text -> Maybe T.Text -> T.Text
showFloor str1 str2 =
  "[" <> ", " `T.intercalate` extractFloor str1 str2 <> "]"

extractFloor :: Maybe T.Text -> Maybe T.Text -> [T.Text]
extractFloor str1 str2 =
  let
    fls1 = extractFloor' str1
    fls2 = extractFloor' str2
    flsr =
      if DL.null fls1
        then fls2
        else fls1
  in
    DL.sort . fmap (fixFloorWords . T.unpack) $ flsr
  where
    extractFloor' :: Maybe T.Text -> [T.Text]
    extractFloor' Nothing = []
    extractFloor' (Just str') =
      let
        wordsStr =   T.toLower
          . HT.cleanString
          . HT.cleanExcessSpaces
          . HNT.keepOnlyLettersAndNums'
          $ str'
        wss = T.words wordsStr
        resws = DL.filter (`elem` wss) extractFloorWords

        floorStr = HT.cleanText'
          . fmap HT.cleanExcessSpaces
          . HNT.keepOnlyLettersAndNums
          . T.split (==',')
          . T.map (\ch ->
            if ch == '('
            || ch == ')'
            || ch == '-'
            || ch == '–'
            || ch == ';'
            || ch == ':' then ',' else ch)
          . T.replace "этаже" "этаж"
          . T.replace "-ый" T.empty
          . T.replace "-ом" T.empty
          . T.replace "помещение на" ","
          . T.replace "жилого дома" ","
          . T.replace "в жилом кирпичном доме" ","
          . T.replace "в жилом доме" ","
          . T.replace "в жилом доме на" ","
          . T.replace "в доме" ","
          . T.replace "квартира на" ","
          . T.replace "в комплексе" ","
          $ str'
        flrss = fmap ("этаж " <>) . mapMaybe tryGetFloor $ floorStr
      in
        resws <> flrss

    tryGetFloor :: T.Text -> Maybe T.Text
    tryGetFloor tryStr =
      let
        ws = T.words tryStr
      in
        if "этаж" `notElem` ws || DL.length ws /= 2
          then Nothing
          else
            let
              fltxt = head . filter (/= "этаж") $ ws
              fch = T.uncons fltxt
              res
                | isNothing fch = Nothing
                | C.isDigit . fst . fromJust $ fch = Just fltxt
                | otherwise = Nothing
            in
              res

extractFloorWords :: [T.Text]
extractFloorWords = ["подвал", "полуподвал", "цоколь", "мезонин",
  "цокольный", "подвале", "техподполье", "подполье"]

fixFloorWords :: String -> T.Text
fixFloorWords "цокольный" = "цоколь"
fixFloorWords "подвале" = "подвал"
fixFloorWords "подполье" = "техподполье"
fixFloorWords w = T.pack w

getRoomsCountDL :: Maybe T.Text -> Maybe T.Text -> Maybe Int
getRoomsCountDL Nothing Nothing = Nothing
getRoomsCountDL (Just d) Nothing = getRoomsCount d
getRoomsCountDL Nothing (Just l) = getRoomsCount l
getRoomsCountDL (Just d) (Just l) =
  let
    d' = getRoomsCount d
    l' = getRoomsCount l
    res
      | isJust d' = d'
      | otherwise = l'
  in
    res

getRoomsCount :: T.Text -> Maybe Int
getRoomsCount rmsTxt' =
  let
    rmsTxt =
        HT.cleanExcessSpaces
      . cleanString4Rooms
      . T.toLower
      $ rmsTxt'
    rmss =
      fmap (\w -> getRoomsCountAll w . (`T.breakOnAll` rmsTxt) $ w)
      getRoomsCountWords
    rmscount = sum rmss
    res
      | rmscount == 0 = Nothing
      | otherwise = Just rmscount
  in
    res
  where
    getRoomsCountAll :: T.Text -> [(T.Text, T.Text)] -> Int
    getRoomsCountAll wsep =
        sum
      . fmap getRoomsCountOne
      . mapMaybe (T.stripPrefix wsep . snd)

    getRoomsCountOne :: T.Text -> Int
    getRoomsCountOne =
        sum
      . fmap getRoomsCountCompute
      . (\(ts1, ts2) ->
          if DL.null ts2
            then ts1
            else
              ts1 <> [head . T.words . head $ ts2]
        )
      . DL.span ((==) 1 . DL.length . T.words)
      . DL.takeWhile (C.isDigit . T.head)
      . HT.cleanText'
      . fmap HNT.removeBrackets
      . T.split (==',')

    getRoomsCountCompute :: T.Text -> Int
    getRoomsCountCompute rmsTxtComp =
      let
        ws = T.split (=='-') rmsTxtComp
      in
        if DL.length ws == 1
          then 1
          else
            getRoomsCountComputeRange ws

    getRoomsCountComputeRange :: [T.Text] -> Int
    getRoomsCountComputeRange rnge =
      let
        nms1 = (reads :: String -> [(Int, String)]) . T.unpack . head $ rnge
        nms2 = (reads :: String -> [(Int, String)]) . T.unpack . last $ rnge
      in
        case nms1 of
          [(nm1, _)] ->
            case nms2 of
              [(nm2, _)] ->
                let
                  c = nm2 - nm1
                  res
                    | c > 0 = c + 1
                    | otherwise = 0
                in
                  res
              _ -> 0
          _ -> 0

getRoomsCountWords :: [T.Text]
getRoomsCountWords =
  ["комнаты ", "комната ", "ком ", "комн.", "ком.", "комн ", "к. "]

cleanString4Rooms :: T.Text -> T.Text
cleanString4Rooms =
  T.map (\c ->
    if C.isLetter c
      || C.isDigit c
      || c == '-'
      || c == '.'
      || c == ','
      || c == '('
      || c == ')'
      then c
      else ' ')

extractStreetAddr :: Maybe T.Text -> Maybe T.Text -> IO (Maybe AddrStreet)
extractStreetAddr str1 str2 =
  extractStreetAddrTries
    [ extractStreetAddrTry1 str1
    , extractStreetAddrTry1 str2
    ]
  where
    extractStreetAddrTry1 :: Maybe T.Text -> IO (Maybe AddrStreet)
    extractStreetAddrTry1 Nothing = return Nothing
    extractStreetAddrTry1 (Just str) = do
      let ss = T.splitOn "," str
          name' = extractStretName ss
          name = cleanStreet . HT.cleanString $ name'
          st = detectStreetType name
      case st of
        Nothing -> return Nothing
        Just (strType, strName') -> do
          let strName = corrStreetName strName'
          strEnt <- dbConn $ selectFirst
                      [ AddrStreetFormalName ==. fixStreetName strName
                      , AddrStreetShortName ==. strType
                      ] []
          case strEnt of
            Nothing -> return Nothing
            Just (Entity _ rec) -> return . Just $ rec

    fixStreetName :: T.Text -> T.Text
    fixStreetName nm = fixStreetName' . T.unpack $ nm
      where
        fixStreetName' "Героев-Панфиловцев" = "Героев Панфиловцев"
        fixStreetName' _ = nm

    cleanStreet :: T.Text -> T.Text
    cleanStreet = T.unwords . cleanStreet' . T.words
      where
        stopStreetPart :: T.Text -> Bool
        stopStreetPart t =
          let
            chk1 = "д." `T.isPrefixOf` t
            chk2 = "д" == t
            chks = [chk1, chk2]
          in
            True `DL.elem` chks

        cleanStreet' :: [T.Text] -> [T.Text]
        cleanStreet' [] = []
        cleanStreet' (t:ts) =
          if stopStreetPart t
            then []
            else t : cleanStreet' ts

    extractStretName :: [T.Text] -> T.Text
    extractStretName [] = T.empty
    extractStretName (np':nps) =
      let
        np = T.toLower . HT.cleanString $ np'
      in
        if DL.any (\f -> f np) [chk1, chk2, chk3, chk4]
          then extractStretName nps
          else np'
      where
        chk1 :: T.Text -> Bool
        chk1 = ("москва" `T.isInfixOf`)

        chk2 :: T.Text -> Bool
        chk2 = ("россия" `T.isInfixOf`)

        chk3 :: T.Text -> Bool
        chk3 = T.all C.isDigit

        chk4 :: T.Text -> Bool
        chk4 = ("р-н" `T.isPrefixOf`)

    corrStreetName :: T.Text -> T.Text
    corrStreetName str =
      corrStreetNameTries
        (T.unpack str)
        [ corrStreetNameTry1
        , corrStreetNameTry2
        , corrStreetNameTry3
        , corrStreetNameTry4
        ]
      where
        corrStreetNameTry4 :: String -> Maybe T.Text
        corrStreetNameTry4 street =
          let
            ws = DL.words street
          in
            if DL.length ws < 2
              then Nothing
              else
                let
                  fp = head ws
                  ifn = "Новая" `DL.isPrefixOf` fp || "Новый" `DL.isPrefixOf` fp
                  ifs = "Старая" `DL.isPrefixOf` fp || "Старый" `DL.isPrefixOf` fp
                in
                  if ifn || ifs
                    then Just . T.pack $
                      DL.unwords (DL.tail ws <> [
                        if ifn then "Нов." else "Стар."])
                    else Nothing

        corrStreetNameTry3 :: String -> Maybe T.Text
        corrStreetNameTry3 street =
          let
            ws = DL.words street
          in
            if DL.length ws < 2
              then Nothing
              else
                let
                  fp = head ws
                  ifm = "Малый" `DL.isPrefixOf` fp || "Малая" `DL.isPrefixOf` fp
                  ifb = "Большой" `DL.isPrefixOf` fp || "Большая" `DL.isPrefixOf` fp
                in
                  if ifm || ifb
                    then Just . T.pack $
                      DL.unwords (DL.tail ws <> [
                        if ifm then "М." else "Б."])
                    else Nothing

        corrStreetNameTry2 :: String -> Maybe T.Text
        corrStreetNameTry2 street =
          let
            ws = DL.words street
          in
            if DL.length ws < 2
              then Nothing
              else
                let
                  fp = head ws
                  ifv = "Верхняя" `DL.isPrefixOf` fp || "Верхний" `DL.isPrefixOf` fp
                  ifn = "Нижняя" `DL.isPrefixOf` fp || "Нижний" `DL.isPrefixOf` fp
                in
                  if ifv || ifn
                    then Just . T.pack $
                      DL.unwords (DL.tail ws <> [
                        if ifv then "Верхн." else "Нижн."])
                    else Nothing

        corrStreetNameTry1 :: String -> Maybe T.Text
        corrStreetNameTry1 street =
          let
            ws = DL.words street
          in
            if DL.length ws < 2
              then Nothing
              else
                let
                  fp = head ws
                in
                  if "-й" `DL.isSuffixOf` fp || "-я" `DL.isSuffixOf` fp
                    then Just . T.pack $ DL.unwords (DL.tail ws <> [fp])
                    else Nothing

        corrStreetNameTries :: String -> [String -> Maybe T.Text] -> T.Text
        corrStreetNameTries street [] = T.pack street
        corrStreetNameTries street (cs:ts) =
          fromMaybe (corrStreetNameTries street ts) (cs street)

    xmlStreetTypes :: [T.Text]
    xmlStreetTypes =
      ["ул.", "улица", "проезд", "пр.", "пр-д", "проспект",
      "просп.", "пр-т", "пер.", "бульвар", "переулок", "ш.",
      "шоссе", "пл.", "площадь"]

    xmlToDBStreetType :: String -> Maybe T.Text
    xmlToDBStreetType "ул." = Just "ул"
    xmlToDBStreetType "улица" = Just "ул"
    xmlToDBStreetType "проезд" = Just "проезд"
    xmlToDBStreetType "пр." = Just "проезд"
    xmlToDBStreetType "пр-д" = Just "проезд"
    xmlToDBStreetType "проспект" = Just "пр-кт"
    xmlToDBStreetType "просп." = Just "пр-кт"
    xmlToDBStreetType "пр-т" = Just "пр-кт"
    xmlToDBStreetType "пер." = Just "пер"
    xmlToDBStreetType "переулок" = Just "пер"
    xmlToDBStreetType "бульвар" = Just "б-р"
    xmlToDBStreetType "ш." = Just "ш"
    xmlToDBStreetType "шоссе" = Just "ш"
    xmlToDBStreetType "пл." = Just "пл"
    xmlToDBStreetType "площадь" = Just "пл"
    xmlToDBStreetType _ = Nothing

    detectStreetType :: T.Text -> Maybe (T.Text, T.Text)
    detectStreetType snm =
      let
        psType = detectStreetType'
      in
        if isNothing psType
          then Nothing
          else
            let
              dbt = xmlToDBStreetType . T.unpack . fromJust $ psType
            in
              if isNothing dbt
                then Nothing
                else
                  let
                    dbt' = fromJust dbt
                    psType' = fromJust psType
                    snm' = HT.cleanString . (psType' `HT.stripAroundText`) $ snm
                  in
                    Just (dbt', snm')
      where
        detectStreetType' :: Maybe T.Text
        detectStreetType' =
          let pst = filter (
                \t -> t `T.isSuffixOf` snm
                   || t `T.isPrefixOf` snm)
                   xmlStreetTypes
          in
            if DL.null pst
              then Nothing
              else Just . DL.head $ pst

    extractStreetAddrTries :: [IO (Maybe AddrStreet)] -> IO (Maybe AddrStreet)
    extractStreetAddrTries [] = return Nothing
    extractStreetAddrTries (t:ts) = do
      r <- t
      if isJust r
        then return r
        else extractStreetAddrTries ts

getHouseByAddr :: AddrStreet -> AddrPlace -> IO (Maybe AddrHouse)
getHouseByAddr as apl = do
  let guid = addrStreetGuid as
  house <- dbConn $ selectList
    ((AddrHouseGuid ==. guid) : mkAddrPlaceFilters apl) []
  TIO.putStrLn $ "Street GUID: " <> guid
  if DL.null house
    then return Nothing
    else return . Just . entityVal . head $ house

mkAddrPlaceFilters :: AddrPlace -> [Filter AddrHouse]
mkAddrPlaceFilters (AddrPlace house build struc) =
  catMaybes
   [ mkHouseFilter house
   , mkBuildFilter build
   , mkStrucFilter struc
   ]
  where
    mkStrucFilter :: Maybe AddrStrucNum -> Maybe (Filter AddrHouse)
    mkStrucFilter Nothing =
      Just (AddrHouseStrNum ==. T.empty)
    mkStrucFilter (Just (AddrStrucNum s)) =
      Just (AddrHouseStrNum ==. s)

    mkBuildFilter :: Maybe AddrBuildNum -> Maybe (Filter AddrHouse)
    mkBuildFilter Nothing =
      Just (AddrHouseBuildNum ==. T.empty)
    mkBuildFilter (Just (AddrBuildNum b)) =
      Just (AddrHouseBuildNum ==. b)

    mkHouseFilter :: Maybe AddrHouseNum -> Maybe (Filter AddrHouse)
    mkHouseFilter Nothing =
      Just (AddrHouseHouseNum ==. T.empty)
    mkHouseFilter (Just (AddrHouseNum h)) =
      Just (AddrHouseHouseNum ==. h)

isAddrPlaceEmpty :: AddrPlace -> Bool
isAddrPlaceEmpty (AddrPlace Nothing Nothing Nothing) = True
isAddrPlaceEmpty _ = False

parseAddPlace :: Maybe T.Text -> AddrPlace
parseAddPlace Nothing = AddrPlace Nothing Nothing Nothing
parseAddPlace (Just addrStr') =
  let
    addrStr =
        HT.cleanExcessSpaces
      . HT.cleanString
      . HT.hideCRLF
      . replaceAChars
      . T.toLower $ addrStr'
    addrHouseNum = parseHouse addrStr T.empty
    addrBuildNum = parseBuild addrStr T.empty
    addrStrucNum = parseStruc addrStr T.empty
  in
    AddrPlace addrHouseNum addrBuildNum addrStrucNum
  where
    replaceAChars :: T.Text -> T.Text
    replaceAChars = T.map replaceChar
      where
        replaceChar :: Char -> Char
        replaceChar 'A' = 'А'
        replaceChar 'O' = 'О'
        replaceChar 'E' = 'Е'
        replaceChar ch = ch

    parseStruc :: T.Text -> T.Text -> Maybe AddrStrucNum
    parseStruc str temp =
      if DL.any
          (`T.isSuffixOf` temp)
          ["стр.", "строен.", "соор.", "строение"]
        then
          let
            str' = HT.cleanString str
            res' = T.takeWhile C.isDigit str'
            res  =
              if T.null res'
                then T.empty
                else res'
                  <> T.takeWhile
                        (\ch ->
                                C.isDigit ch
                             || C.isLetter ch
                             || ch `DL.elem` ['/','-'])
                        (T.drop (T.length res') str')
          in
            if T.null res
              then parseNext
              else Just . AddrStrucNum . T.toUpper $ res
        else
          parseNext
      where
        parseNext :: Maybe AddrStrucNum
        parseNext =
          case T.uncons str of
            Just (ch, nextStr) ->
              parseStruc nextStr (temp `T.snoc` ch)
            _ -> Nothing

    parseBuild :: T.Text -> T.Text -> Maybe AddrBuildNum
    parseBuild str temp =
      if DL.any (`T.isSuffixOf` temp) ["корп.", "кор."]
        then
          let
            str' = HT.cleanString str
            res  =
              T.takeWhile
                (\ch ->
                        C.isDigit ch
                     || C.isLetter ch
                     || ch `DL.elem` ['/','-'])
                str'
          in
            if T.null res
              then Nothing
              else Just . AddrBuildNum . T.toUpper $ res
        else
          case T.uncons str of
            Just (ch, nextStr) ->
              parseBuild nextStr (temp `T.snoc` ch)
            _ -> Nothing

    parseHouse :: T.Text -> T.Text -> Maybe AddrHouseNum
    parseHouse str temp =
      if DL.any
          (`T.isSuffixOf` temp)
          ["д.", "домовлад.", "вл.", "дом", "домовладение"]
        then
          let
            str' = HT.cleanString str
            res' = T.takeWhile C.isDigit str'
            res  =
              if T.null res'
                then T.empty
                else res'
                  <> T.takeWhile
                        (\ch ->
                                C.isDigit ch
                             || C.isLetter ch
                             || ch `DL.elem` ['/','-'])
                        (T.drop (T.length res') str')
          in
            if T.null res
              then parseNext
              else
                let
                  rest = HT.cleanString . T.drop (T.length res) $ str'
                  liter = getHouseLiter . T.unpack $ rest
                in
                  Just . AddrHouseNum . T.toUpper $ res <> liter
        else
          parseNext
      where
        getHouseLiter :: String -> T.Text
        getHouseLiter (c1:c2:_) =
          if C.isLetter c1 && not (C.isLetter c2)
            then T.pack [c1]
            else T.empty
        getHouseLiter _ = T.empty

        parseNext :: Maybe AddrHouseNum
        parseNext =
          case T.uncons str of
            Just (ch, nextStr) ->
              parseHouse nextStr (temp `T.snoc` ch)
            _ -> Nothing

printDocuments :: Maybe [NDetailsDocument] -> IO ()
printDocuments (Just docs) = forM_ docs printDocument
  where
    printDocument :: NDetailsDocument -> IO ()
    printDocument doc = do
      maybe (return ()) TIO.putStrLn . nDetailsDocumentDocType $ doc
      maybe (return ()) TIO.putStrLn . nDetailsDocumentDocDate $ doc
      maybe (return ()) TIO.putStrLn . nDetailsDocumentCreated $ doc
      maybe (return ()) putStrLnNE . nDetailsDocumentDescription $ doc
      maybe (return ()) (TIO.putStrLn . stripCDATA) . nDetailsDocumentDocUrl $ doc
      maybe (return ()) (printURLFilename . stripCDATA) . nDetailsDocumentDocUrl $ doc
      putStrLn "################################"
      putStrLn ""

printDocuments _ = return ()

printURLFilename :: T.Text -> IO ()
printURLFilename url = do
  (fName, fURL) <- getUrlFilename url
  maybe (return ()) TIO.putStrLn fURL
  maybe (return ()) TIO.putStrLn fName

getUrlFilename :: T.Text -> IO (Maybe T.Text, Maybe T.Text)
getUrlFilename url = do
  -- (_, urlContent) <- HN.getContentsByURL userAgent . T.unpack $ url
  httC <- try @SomeException (HN.getContentsByURL userAgent . T.unpack $ url)
  if isLeft httC
    then do
      let (Left exception) = httC
      putStrLn ("http error: " <> show exception)
      return (Nothing, Nothing)
    else do
      let Right (_, urlContent) = httC
      let
        doc :: IOSArrow XmlTree XmlTree
        doc = readString
                [withParseHTML yes, withWarnings no, withInputEncoding utf8]
                (SU8.toString urlContent)
      fs <- filter (T.isInfixOf "filename=") . fmap T.pack
        <$> runX (doc >>> css "a" >>> getAttrValue "href")
      let
        fURL =
          if DL.null fs
            then Nothing
            else Just . addPrefixFilename . head $ fs
      return (maybe Nothing getFilenameFromQuery fURL, fURL)
      where
        addPrefixFilename :: T.Text -> T.Text
        addPrefixFilename = T.replace "../resources/" "https://torgi.gov.ru/resources/"

        getFilenameFromQuery :: T.Text -> Maybe T.Text
        getFilenameFromQuery fileURL =
          let
            qs = HN.getUrlQueryParams fileURL
          in
            if isNothing qs
              then Nothing
              else getFilename . DL.filter ((==) "filename" . fst) . fromJust $ qs
          where
            getFilename :: HN.QueryText -> Maybe T.Text
            getFilename [] = Nothing
            getFilename (q:_) = snd q

putStrLnNE :: T.Text -> IO ()
putStrLnNE str =
  CM.unless (T.null str) $
    TIO.putStrLn str

stripCDATA :: T.Text -> T.Text
stripCDATA str =
  let txtS = fromMaybe str (T.stripPrefix "<![CDATA[" str)
      txtE = fromMaybe txtS (T.stripSuffix "]]>" txtS)
  in txtE

filterRentPremisesFreshAuc :: UTCTime -> Maybe NDetailsCommon -> Bool
filterRentPremisesFreshAuc aucChkTime common =
  let
    bidAuctionDate = nDetailsCommonBidAuctionDate . fromJust $ common
  in
    (isJust bidAuctionDate &&
      (filterRentPremisesFreshAuc' . fromJust $ bidAuctionDate))
  where
    filterRentPremisesFreshAuc' :: T.Text -> Bool
    filterRentPremisesFreshAuc' aucTxtTm =
      let
        aucTm = HDT.uniDateTimeToUTC . T.unpack $ aucTxtTm
      in
        (isJust aucTm &&
          (filterRentPremisesFreshAuc'' . fromJust $ aucTm))
    filterRentPremisesFreshAuc'' :: UTCTime -> Bool
    filterRentPremisesFreshAuc'' aucTm = aucTm >= aucChkTime

filterRentPremisesProperty :: Maybe NotificationFull -> Bool
filterRentPremisesProperty Nothing = False
filterRentPremisesProperty (Just nFull) =
  let
    details = notificationFullDetails nFull
    lots = nDetailsLots details
  in
    DL.any isfilterRentPremisesProperty lots

isfilterRentPremisesProperty :: NDetailsLot -> Bool
isfilterRentPremisesProperty lot =
  let
    propertyType = nDetailsLotPropertyType lot
  in
    (isJust propertyType &&
      (isPropertyType . fromJust $ propertyType))

isPropertyType :: NDetailsLotPropertyType -> Bool
isPropertyType propertyType =
  let
    name = nDetailsLotPropertyTypeName propertyType
  in
    (isJust name &&
      (T.isPrefixOf "Недвижимое имущество;" . fromJust $ name))

filterRentPremisesMoscow :: Maybe NotificationFull -> Bool
filterRentPremisesMoscow Nothing = False
filterRentPremisesMoscow (Just nFull) =
  let
    details = notificationFullDetails nFull
    lots = nDetailsLots details
  in
    DL.any isRentPremisesLotMoscow lots

isRentPremisesLotMoscow :: NDetailsLot -> Bool
isRentPremisesLotMoscow lot =
  let
    kladr = nDetailsLotKladrLocation lot
  in
    (isJust kladr &&
      (isKladrMoscow . fromJust $ kladr))

isKladrMoscow :: NDetailsLotKladrLocation -> Bool
isKladrMoscow kladr =
  let
    _id = nDetailsLotKladrLocationId kladr
  in
    (isJust _id &&
      (T.isPrefixOf "77" . fromJust $ _id))

getNotificationFull :: LotsDir t -> Notification t -> IO (FilePath, Maybe NotificationFull)
getNotificationFull ld notification = do
  let nDir = T.unpack . unLotsDir $ ld
  let nId =
          T.unpack
        . T.takeWhileEnd (/= '/')
        . fromJust
        . notificationOdDetailedHref
        $ notification
  let detailsDir = nDir </> notificationIdToDir nId
  let detailsFile = detailsDir </> (nId <> ".xml")
  fExists <- SD.doesFileExist detailsFile
  if not fExists
    then return (detailsFile, Nothing)
    else do
      fText <- HIO.readFileUtf8 detailsFile
      if "Error 500--Internal Server Error" `T.isInfixOf` fText
        then
          return (detailsFile, Nothing)
        else
          if notificationIsArchived notification /= Just "1"
            then do
              nFull <- runResourceT
                 $ parseFile def detailsFile
                $$ parseNotificationFull
              return (detailsFile, Just nFull)
            else
              return (detailsFile, Nothing)

downloadNs :: LotsUrl t -> LotsFile t -> LotsDir t -> IO ()
downloadNs lu lf ld = do
  (pdu, cdu) <- HDT.subtractYearsFromCurrent 6
  pduS <- T.pack <$> HDT.formatUTCToCurrentLocTime pdu
  cduS <- T.pack <$> HDT.formatUTCToCurrentLocTime cdu
  let
    lotsFilledUrl = fillLotsUrlDates pduS cduS lu
  TIO.putStrLn
    $ "Downloading: "
   <> unLotsUrl lotsFilledUrl
   <> " to "
   <> unLotsFile lf
  downloadLotsXml lotsFilledUrl lf
  let fpDir = T.unpack . unLotsDir $ ld
  dirExists <- SD.doesDirectoryExist fpDir
  unless dirExists $ SD.createDirectory fpDir
  runResourceT
     $ parseFile def (T.unpack $ unLotsFile lf)
    $$ parseOpenData
    =$ CL.mapM_ (lift . downloadNotificationDetails ld)

downloadNotificationDetails :: LotsDir t -> Notification t -> IO ()
downloadNotificationDetails ld notification = do
  let nDir = T.unpack . unLotsDir $ ld
  let nId =
          T.unpack
        . T.takeWhileEnd (/= '/')
        . fromJust
        . notificationOdDetailedHref
        $ notification
  let detailsDir = nDir </> notificationIdToDir nId
  let detailsFile = detailsDir </> (nId <> ".xml")
  let detailsUrl =
          T.unpack
        . fromJust
        . notificationOdDetailedHref
        $ notification
  dirExists <- SD.doesDirectoryExist detailsDir
  unless dirExists $ SD.createDirectory detailsDir
  putStrLn $ "Downloading " <> detailsUrl
  handle @SomeException print
    $ HN.downloadFileByURL detailsUrl detailsFile False

notificationIdToDir :: String -> String
notificationIdToDir nId =
  let divizor = 10000
      n = read @Int nId `quot` divizor in
      if n == 0
        then show divizor
        else show (n * divizor)

parseNotificationFull :: ConduitM Event o (ResourceT IO) NotificationFull
parseNotificationFull = force "fullNotification tag missing"
  . tagName "{http://torgi.gov.ru/opendata}fullNotification" ignoreAttrs
  $ \() -> do
    details <- parseNDetailsNotification
    protocols <- many parseNProtocol
    return
      $ NotificationFull
          details
          protocols

parseNProtocol :: ConduitM Event o (ResourceT IO) (Maybe NProtocol)
parseNProtocol = tagNoAttr "{http://torgi.gov.ru/opendata}protocol" $ do
  protocolType <- tagNoAttr "{http://torgi.gov.ru/opendata}protocolType" content
  protocolNum <- tagNoAttr "{http://torgi.gov.ru/opendata}protocolNum" content
  protocolTown <- tagNoAttr "{http://torgi.gov.ru/opendata}protocolTown" content
  protocolDate <- tagNoAttr "{http://torgi.gov.ru/opendata}protocolDate" content
  protocolPlace <- tagNoAttr "{http://torgi.gov.ru/opendata}protocolPlace" content
  committee <- parseNProtocolCommittee
  lots <- many parseNProtocolLot
  return
    $ NProtocol
        protocolType
        protocolNum
        protocolTown
        protocolDate
        protocolPlace
        committee
        lots

parseNProtocolLot :: ConduitM Event o (ResourceT IO) (Maybe NProtocolLot)
parseNProtocolLot = tagNoAttr "{http://torgi.gov.ru/opendata}lot" $ do
  lotNum <- tagNoAttr "{http://torgi.gov.ru/opendata}lotNum" content
  cancelReason <- tagNoAttr "{http://torgi.gov.ru/opendata}cancelReason" content
  members <- many parseNProtocolLotBidMember
  documents <- tagNoAttr "{http://torgi.gov.ru/opendata}documents" $ many parseNProtocolLotDocument
  decision <- tagNoAttr "{http://torgi.gov.ru/opendata}decision" content
  return
    $ NProtocolLot
        lotNum
        cancelReason
        members
        documents
        decision

parseNProtocolLotDocument :: ConduitM Event o (ResourceT IO) (Maybe NProtocolLotDocument)
parseNProtocolLotDocument = tagNoAttr "{http://torgi.gov.ru/opendata}doc" $ do
    docType <- tagNoAttr "{http://torgi.gov.ru/opendata}docType" content
    description <- tagNoAttr "{http://torgi.gov.ru/opendata}description" content
    created <- tagNoAttr "{http://torgi.gov.ru/opendata}created" content
    docUrl <- tagNoAttr "{http://torgi.gov.ru/opendata}docUrl" content
    return
      $ NProtocolLotDocument
          docType
          description
          created
          docUrl

parseNProtocolLotBidMember :: ConduitM Event o (ResourceT IO) (Maybe NProtocolLotBidMember)
parseNProtocolLotBidMember = tagNoAttr "{http://torgi.gov.ru/opendata}bidMember" $ do
  regNum <- tagNoAttr "{http://torgi.gov.ru/opendata}regNum" content
  name <- tagNoAttr "{http://torgi.gov.ru/opendata}name" content
  inn <- tagNoAttr "{http://torgi.gov.ru/opendata}inn" content
  kpp <- tagNoAttr "{http://torgi.gov.ru/opendata}kpp" content
  ogrn <- tagNoAttr "{http://torgi.gov.ru/opendata}ogrn" content
  location <- tagNoAttr "{http://torgi.gov.ru/opendata}location" content
  phone <- tagNoAttr "{http://torgi.gov.ru/opendata}phone" content
  isRemoved <- tagNoAttr "{http://torgi.gov.ru/opendata}isRemoved" content
  tenderPosition <- tagNoAttr "{http://torgi.gov.ru/opendata}tenderPosition" content
  finalScore <- tagNoAttr "{http://torgi.gov.ru/opendata}finalScore" content
  documents <- tagNoAttr "{http://torgi.gov.ru/opendata}documents" content
  attended <- tagNoAttr "{http://torgi.gov.ru/opendata}attended" content
  offer <- tagNoAttr "{http://torgi.gov.ru/opendata}offer" content
  pricePerAreaSm <- tagNoAttr "{http://torgi.gov.ru/opendata}pricePerAreaSm" content
  isSelected <- tagNoAttr "{http://torgi.gov.ru/opendata}isSelected" content
  refuseReason <- tagNoAttr "{http://torgi.gov.ru/opendata}refuseReason" content
  rejectReason <- tagNoAttr "{http://torgi.gov.ru/opendata}rejectReason" content
  return
    $ NProtocolLotBidMember
        regNum
        name
        inn
        kpp
        ogrn
        location
        phone
        isRemoved
        tenderPosition
        finalScore
        documents
        attended
        offer
        pricePerAreaSm
        isSelected
        refuseReason
        rejectReason

parseNProtocolCommittee :: ConduitM Event o (ResourceT IO) (Maybe NProtocolCommittee)
parseNProtocolCommittee = tagNoAttr "{http://torgi.gov.ru/opendata}committee" $ do
  members <- many parseNProtocolCommitteeMember
  committePercent <- tagNoAttr "{http://torgi.gov.ru/opendata}committePercent" content
  return
    $ NProtocolCommittee
        members
        committePercent

parseNProtocolCommitteeMember :: ConduitM Event o (ResourceT IO) (Maybe NProtocolCommitteeMember)
parseNProtocolCommitteeMember = tagNoAttr "{http://torgi.gov.ru/opendata}member" $ do
  name <- tagNoAttr "{http://torgi.gov.ru/opendata}name" content
  role <- tagNoAttr "{http://torgi.gov.ru/opendata}role" content
  return
    $ NProtocolCommitteeMember
        name
        role

parseNDetailsNotification :: ConduitM Event o (ResourceT IO) NDetails
parseNDetailsNotification = force "notification details missing or icorrect"
  . tagNoAttr "{http://torgi.gov.ru/opendata}notification" $ do
  bidNumber <- tagNoAttr "{http://torgi.gov.ru/opendata}bidNumber" content
  bidOrganization <- tagNoAttr "{http://torgi.gov.ru/opendata}bidOrganization" parseNDetailsBidOrganization
  common <- tagNoAttr "{http://torgi.gov.ru/opendata}common" parseNDetailsCommon
  docs <- tagNoAttr "{http://torgi.gov.ru/opendata}documents" $ many parseNDetailsDocuments
  lots <- many parseNDetailsLots
  return
    $ NDetails
        bidNumber
        bidOrganization
        common
        docs
        lots

parseNDetailsLots :: ConduitM Event o (ResourceT IO) (Maybe NDetailsLot)
parseNDetailsLots = tagNoAttr "{http://torgi.gov.ru/opendata}lot" $ do
  lotNum <- tagNoAttr "{http://torgi.gov.ru/opendata}lotNum" content
  _id <- tagNoAttr "{http://torgi.gov.ru/opendata}id" content
  bidStatus <- parseNDetailsLotBidStatus
  suspendDate <- tagNoAttr "{http://torgi.gov.ru/opendata}suspendDate" content
  suspendReason <- tagNoAttr "{http://torgi.gov.ru/opendata}suspendReason" content
  cancelDate <- tagNoAttr "{http://torgi.gov.ru/opendata}cancelDate" content
  cancelReason <- tagNoAttr "{http://torgi.gov.ru/opendata}cancelReason" content
  bidType <- parseNDetailsLotBidType
  propertyType <- parseNDetailsLotPropertyType
  propKind <- parseNDetailsLotPropKind
  reqDecision <- tagNoAttr "{http://torgi.gov.ru/opendata}reqDecision" content
  cadastralNum <- tagNoAttr "{http://torgi.gov.ru/opendata}cadastralNum" content
  torgReason <- tagNoAttr "{http://torgi.gov.ru/opendata}torgReason" content
  orgFullName <- tagNoAttr "{http://torgi.gov.ru/opendata}orgFullName" content
  propDesc <- tagNoAttr "{http://torgi.gov.ru/opendata}propDesc" content
  objectCode <- tagNoAttr "{http://torgi.gov.ru/opendata}objectCode" content
  description <- tagNoAttr "{http://torgi.gov.ru/opendata}description" content
  mission <- tagNoAttr "{http://torgi.gov.ru/opendata}mission" content
  kladrLocation <- parseNDetailsLotKladrLocation
  postAddress <- tagNoAttr "{http://torgi.gov.ru/opendata}postAddress" content
  fundSize <- tagNoAttr "{http://torgi.gov.ru/opendata}fundSize" content
  orgNominalValue <- tagNoAttr "{http://torgi.gov.ru/opendata}orgNominalValue" content
  acsPart <- tagNoAttr "{http://torgi.gov.ru/opendata}acsPart" content
  location <- tagNoAttr "{http://torgi.gov.ru/opendata}location" content
  unit <- parseNDetailsLotUnit
  area <- tagNoAttr "{http://torgi.gov.ru/opendata}area" content
  description2 <- tagNoAttr "{http://torgi.gov.ru/opendata}description" content
  startSalePrice <- tagNoAttr "{http://torgi.gov.ru/opendata}startSalePrice" content
  condition <- tagNoAttr "{http://torgi.gov.ru/opendata}condition" content
  federalStockPercent <- tagNoAttr "{http://torgi.gov.ru/opendata}federalStockPercent" content
  stockNum <- tagNoAttr "{http://torgi.gov.ru/opendata}stockNum" content
  stockPercentSale <- tagNoAttr "{http://torgi.gov.ru/opendata}stockPercentSale" content
  minPrice <- tagNoAttr "{http://torgi.gov.ru/opendata}minPrice" content
  priceStep <- tagNoAttr "{http://torgi.gov.ru/opendata}priceStep" content
  stepNegative <- tagNoAttr "{http://torgi.gov.ru/opendata}stepNegative" content
  workList <- tagNoAttr "{http://torgi.gov.ru/opendata}workList" content
  marketPartDesc <- tagNoAttr "{http://torgi.gov.ru/opendata}marketPartDesc" content
  areaUnmovable <- tagNoAttr "{http://torgi.gov.ru/opendata}areaUnmovable" content
  objectsList <- tagNoAttr "{http://torgi.gov.ru/opendata}objectsList" content
  emplNum <- tagNoAttr "{http://torgi.gov.ru/opendata}emplNum" content
  docsList <- tagNoAttr "{http://torgi.gov.ru/opendata}docsList" content
  areaMeters <- tagNoAttr "{http://torgi.gov.ru/opendata}areaMeters" content
  termYear <- tagNoAttr "{http://torgi.gov.ru/opendata}termYear" content
  termMonth <- tagNoAttr "{http://torgi.gov.ru/opendata}termMonth" content
  termDay <- tagNoAttr "{http://torgi.gov.ru/opendata}termDay" content
  article <- parseNDetailsLotArticle
  pricePerMonth <- tagNoAttr "{http://torgi.gov.ru/opendata}pricePerMonth" content
  pricePerYear <- tagNoAttr "{http://torgi.gov.ru/opendata}pricePerYear" content
  startPrice <- tagNoAttr "{http://torgi.gov.ru/opendata}startPrice" content
  startPriceAreaSm <- tagNoAttr "{http://torgi.gov.ru/opendata}startPriceAreaSm" content
  step <- tagNoAttr "{http://torgi.gov.ru/opendata}step" content
  monthPrice <- tagNoAttr "{http://torgi.gov.ru/opendata}monthPrice" content
  yearPrice <- tagNoAttr "{http://torgi.gov.ru/opendata}yearPrice" content
  dealFee <- tagNoAttr "{http://torgi.gov.ru/opendata}dealFee" content
  contractFee <- tagNoAttr "{http://torgi.gov.ru/opendata}contractFee" content
  startPricePerMonth <- tagNoAttr "{http://torgi.gov.ru/opendata}startPricePerMonth" content
  isOverLimitDeal <- tagNoAttr "{http://torgi.gov.ru/opendata}isOverLimitDeal" content
  depositSize <- tagNoAttr "{http://torgi.gov.ru/opendata}depositSize" content
  depositDesc2 <- tagNoAttr "{http://torgi.gov.ru/opendata}depositDesc" content
  maintenanceSize <- tagNoAttr "{http://torgi.gov.ru/opendata}maintenanceSize" content
  buildConditions <- tagNoAttr "{http://torgi.gov.ru/opendata}buildConditions" content
  techConditions <- tagNoAttr "{http://torgi.gov.ru/opendata}techConditions" content
  isBurdened <- tagNoAttr "{http://torgi.gov.ru/opendata}isBurdened" content
  burdenDescription <- tagNoAttr "{http://torgi.gov.ru/opendata}burdenDescription" content
  depositSize2 <- tagNoAttr "{http://torgi.gov.ru/opendata}depositSize" content
  depositDesc <- tagNoAttr "{http://torgi.gov.ru/opendata}depositDesc" content
  contractDesc <- tagNoAttr "{http://torgi.gov.ru/opendata}contractDesc" content
  limit <- tagNoAttr "{http://torgi.gov.ru/opendata}limit" content
  winnerDefineDesc <- tagNoAttr "{http://torgi.gov.ru/opendata}winnerDefineDesc" content
  privateConditions <- tagNoAttr "{http://torgi.gov.ru/opendata}privateConditions" content
  lastInfo <- tagNoAttr "{http://torgi.gov.ru/opendata}lastInfo" content
  singlePrice <- tagNoAttr "{http://torgi.gov.ru/opendata}singlePrice" content
  finalPrice <- tagNoAttr "{http://torgi.gov.ru/opendata}finalPrice" content
  result <- tagNoAttr "{http://torgi.gov.ru/opendata}result" content
  isSubrent <- tagNoAttr "{http://torgi.gov.ru/opendata}isSubrent" content
  lotPhotosExist <- tagNoAttr "{http://torgi.gov.ru/opendata}lotPhotosExist" content
  groundViewPlace <- tagNoAttr "{http://torgi.gov.ru/opendata}groundViewPlace" content
  articleVal <- tagNoAttr "{http://torgi.gov.ru/opendata}articleVal" content
  resultStartPriceAreaSm <- tagNoAttr "{http://torgi.gov.ru/opendata}resultStartPriceAreaSm" content
  bidResults <- tagNoAttr "{http://torgi.gov.ru/opendata}bidResults" content
  contractNum <- tagNoAttr "{http://torgi.gov.ru/opendata}contractNum" content
  contractDate <- tagNoAttr "{http://torgi.gov.ru/opendata}contractDate" content
  contractPayment <- tagNoAttr "{http://torgi.gov.ru/opendata}contractPayment" content
  contractPriceYear <- tagNoAttr "{http://torgi.gov.ru/opendata}contractPriceYear" content
  contractPriceMonth <- tagNoAttr "{http://torgi.gov.ru/opendata}contractPriceMonth" content
  contractPriceHour <- tagNoAttr "{http://torgi.gov.ru/opendata}contractPriceHour" content
  winner <- parseNDetailsLotWinner
  currency <- tagNoAttr "{http://torgi.gov.ru/opendata}currency" content
  currencyPercent <- tagNoAttr "{http://torgi.gov.ru/opendata}currencyPercent" content
  paymentRequisites <- parseNDetailsLotPaymentRequisites
  documents <- tagNoAttr "{http://torgi.gov.ru/opendata}documents" $ many parseNDetailsLotDocument
  results <- tagNoAttr "{http://torgi.gov.ru/opendata}results" $ many parseNDetailsLotResultsMember
  return
    $ NDetailsLot
        lotNum
        _id
        bidStatus
        suspendDate
        suspendReason
        cancelDate
        cancelReason
        bidType
        propertyType
        propKind
        reqDecision
        cadastralNum
        torgReason
        orgFullName
        propDesc
        objectCode
        description
        mission
        kladrLocation
        postAddress
        fundSize
        orgNominalValue
        acsPart
        location
        unit
        area
        description2
        startSalePrice
        condition
        federalStockPercent
        stockNum
        stockPercentSale
        minPrice
        priceStep
        stepNegative
        workList
        marketPartDesc
        areaUnmovable
        objectsList
        emplNum
        docsList
        areaMeters
        termYear
        termMonth
        termDay
        article
        pricePerMonth
        pricePerYear
        startPrice
        startPriceAreaSm
        step
        monthPrice
        yearPrice
        dealFee
        contractFee
        startPricePerMonth
        isOverLimitDeal
        depositSize
        depositDesc2
        maintenanceSize
        buildConditions
        techConditions
        isBurdened
        burdenDescription
        depositSize2
        depositDesc
        contractDesc
        limit
        winnerDefineDesc
        privateConditions
        lastInfo
        singlePrice
        finalPrice
        result
        isSubrent
        lotPhotosExist
        groundViewPlace
        articleVal
        resultStartPriceAreaSm
        bidResults
        contractNum
        contractDate
        contractPayment
        contractPriceYear
        contractPriceMonth
        contractPriceHour
        winner
        currency
        currencyPercent
        paymentRequisites
        documents
        results

parseNDetailsLotResultsMember :: ConduitM Event o (ResourceT IO) (Maybe NDetailsLotResultsMember)
parseNDetailsLotResultsMember = tagNoAttr "{http://torgi.gov.ru/opendata}bidMember" $ do
  regNum <- tagNoAttr "{http://torgi.gov.ru/opendata}regNum" content
  name <- tagNoAttr "{http://torgi.gov.ru/opendata}name" content
  inn <- tagNoAttr "{http://torgi.gov.ru/opendata}inn" content
  kpp <- tagNoAttr "{http://torgi.gov.ru/opendata}kpp" content
  ogrnip <- tagNoAttr "{http://torgi.gov.ru/opendata}ogrnip" content
  ogrn <- tagNoAttr "{http://torgi.gov.ru/opendata}ogrn" content
  location <- tagNoAttr "{http://torgi.gov.ru/opendata}location" content
  phone <- tagNoAttr "{http://torgi.gov.ru/opendata}phone" content
  isRemoved <- tagNoAttr "{http://torgi.gov.ru/opendata}isRemoved" content
  isSelected <- tagNoAttr "{http://torgi.gov.ru/opendata}isSelected" content
  refuseReason <- tagNoAttr "{http://torgi.gov.ru/opendata}refuseReason" content
  tenderPosition <- tagNoAttr "{http://torgi.gov.ru/opendata}tenderPosition" content
  contractTearms <- tagNoAttr "{http://torgi.gov.ru/opendata}contractTearms" content
  offer <- tagNoAttr "{http://torgi.gov.ru/opendata}offer" content
  return
    $ NDetailsLotResultsMember
        regNum
        name
        inn
        kpp
        ogrnip
        ogrn
        location
        phone
        isRemoved
        isSelected
        refuseReason
        tenderPosition
        contractTearms
        offer

parseNDetailsLotDocument :: ConduitM Event o (ResourceT IO) (Maybe NDetailsLotDocument)
parseNDetailsLotDocument = tagNoAttr "{http://torgi.gov.ru/opendata}doc" $ do
    docType <- tagNoAttr "{http://torgi.gov.ru/opendata}docType" content
    docDate <- tagNoAttr "{http://torgi.gov.ru/opendata}docDate" content
    docNum <- tagNoAttr "{http://torgi.gov.ru/opendata}docNum" content
    description <- tagNoAttr "{http://torgi.gov.ru/opendata}description" content
    created <- tagNoAttr "{http://torgi.gov.ru/opendata}created" content
    docUrl <- tagNoAttr "{http://torgi.gov.ru/opendata}docUrl" content
    return
      $ NDetailsLotDocument
          docType
          docDate
          docNum
          description
          created
          docUrl

parseNDetailsLotPaymentRequisites :: ConduitM Event o (ResourceT IO) (Maybe NDetailsLotPaymentRequisites)
parseNDetailsLotPaymentRequisites = tagNoAttr "{http://torgi.gov.ru/opendata}paymentRequisites" $ do
  bik <- tagNoAttr "{http://torgi.gov.ru/opendata}bik" content
  bankName <- tagNoAttr "{http://torgi.gov.ru/opendata}bankName" content
  ks <- tagNoAttr "{http://torgi.gov.ru/opendata}ks" content
  rs <- tagNoAttr "{http://torgi.gov.ru/opendata}rs" content
  ps <- tagNoAttr "{http://torgi.gov.ru/opendata}ps" content
  return
    $ NDetailsLotPaymentRequisites
        bik
        bankName
        ks
        rs
        ps

parseNDetailsLotWinner :: ConduitM Event o (ResourceT IO) (Maybe NDetailsLotWinner)
parseNDetailsLotWinner = tagNoAttr "{http://torgi.gov.ru/opendata}winner" $ do
  name <- tagNoAttr "{http://torgi.gov.ru/opendata}name" content
  inn <- tagNoAttr "{http://torgi.gov.ru/opendata}inn" content
  kpp <- tagNoAttr "{http://torgi.gov.ru/opendata}kpp" content
  ogrnip <- tagNoAttr "{http://torgi.gov.ru/opendata}ogrnip" content
  ogrn <- tagNoAttr "{http://torgi.gov.ru/opendata}ogrn" content
  location <- tagNoAttr "{http://torgi.gov.ru/opendata}location" content
  phone <- tagNoAttr "{http://torgi.gov.ru/opendata}phone" content
  return
    $ NDetailsLotWinner
        name
        inn
        kpp
        ogrnip
        ogrn
        location
        phone

parseNDetailsLotArticle :: ConduitM Event o (ResourceT IO) (Maybe NDetailsLotArticle)
parseNDetailsLotArticle = tagNoAttr "{http://torgi.gov.ru/opendata}article" $ do
  _id <- tagNoAttr "{http://torgi.gov.ru/opendata}id" content
  name <- tagNoAttr "{http://torgi.gov.ru/opendata}name" content
  return
    $ NDetailsLotArticle
        _id
        name

parseNDetailsLotKladrLocation :: ConduitM Event o (ResourceT IO) (Maybe NDetailsLotKladrLocation)
parseNDetailsLotKladrLocation = tagNoAttr "{http://torgi.gov.ru/opendata}kladrLocation" $ do
  _id <- tagNoAttr "{http://torgi.gov.ru/opendata}id" content
  name <- tagNoAttr "{http://torgi.gov.ru/opendata}name" content
  return
    $ NDetailsLotKladrLocation
        _id
        name

parseNDetailsLotPropKind :: ConduitM Event o (ResourceT IO) (Maybe NDetailsLotPropKind)
parseNDetailsLotPropKind = tagNoAttr "{http://torgi.gov.ru/opendata}propKind" $ do
  _id <- tagNoAttr "{http://torgi.gov.ru/opendata}id" content
  name <- tagNoAttr "{http://torgi.gov.ru/opendata}name" content
  return
    $ NDetailsLotPropKind
        _id
        name

parseNDetailsLotUnit :: ConduitM Event o (ResourceT IO) (Maybe NDetailsLotUnit)
parseNDetailsLotUnit = tagNoAttr "{http://torgi.gov.ru/opendata}unit" $ do
  _id <- tagNoAttr "{http://torgi.gov.ru/opendata}id" content
  name <- tagNoAttr "{http://torgi.gov.ru/opendata}name" content
  return
    $ NDetailsLotUnit
        _id
        name

parseNDetailsLotPropertyType :: ConduitM Event o (ResourceT IO) (Maybe NDetailsLotPropertyType)
parseNDetailsLotPropertyType = tagNoAttr "{http://torgi.gov.ru/opendata}propertyType" $ do
  _id <- tagNoAttr "{http://torgi.gov.ru/opendata}id" content
  name <- tagNoAttr "{http://torgi.gov.ru/opendata}name" content
  return
    $ NDetailsLotPropertyType
        _id
        name

parseNDetailsLotBidType :: ConduitM Event o (ResourceT IO) (Maybe NDetailsLotBidType)
parseNDetailsLotBidType = tagNoAttr "{http://torgi.gov.ru/opendata}bidType" $ do
  _id <- tagNoAttr "{http://torgi.gov.ru/opendata}id" content
  name <- tagNoAttr "{http://torgi.gov.ru/opendata}name" content
  return
    $ NDetailsLotBidType
        _id
        name

parseNDetailsLotBidStatus :: ConduitM Event o (ResourceT IO) (Maybe NDetailsLotBidStatus)
parseNDetailsLotBidStatus = tagNoAttr "{http://torgi.gov.ru/opendata}bidStatus" $ do
  _id <- tagNoAttr "{http://torgi.gov.ru/opendata}id" content
  name <- tagNoAttr "{http://torgi.gov.ru/opendata}name" content
  return
    $ NDetailsLotBidStatus
        _id
        name

parseNDetailsDocuments :: ConduitM Event o (ResourceT IO) (Maybe NDetailsDocument)
parseNDetailsDocuments = tagNoAttr "{http://torgi.gov.ru/opendata}doc" $ do
    docType <- tagNoAttr "{http://torgi.gov.ru/opendata}docType" content
    docDate <- tagNoAttr "{http://torgi.gov.ru/opendata}docDate" content
    description <- tagNoAttr "{http://torgi.gov.ru/opendata}description" content
    created <- tagNoAttr "{http://torgi.gov.ru/opendata}created" content
    docUrl <- tagNoAttr "{http://torgi.gov.ru/opendata}docUrl" content
    return
      $ NDetailsDocument
          docType
          docDate
          description
          created
          docUrl


parseNDetailsCommon :: ConduitM Event o (ResourceT IO) NDetailsCommon
parseNDetailsCommon = do
  bidKind <- tagNoAttr "{http://torgi.gov.ru/opendata}bidKind" parseNDetailsCommonBidKind
  bidForm <- tagNoAttr "{http://torgi.gov.ru/opendata}bidForm" parseNDetailsCommonBidForm
  bidUrl <- tagNoAttr "{http://torgi.gov.ru/opendata}bidUrl" content
  fio <- tagNoAttr "{http://torgi.gov.ru/opendata}fio" content
  lotNum <- tagNoAttr "{http://torgi.gov.ru/opendata}lotNum" content
  published <- tagNoAttr "{http://torgi.gov.ru/opendata}published" content
  lastChanged <- tagNoAttr "{http://torgi.gov.ru/opendata}lastChanged" content
  timeOut <- tagNoAttr "{http://torgi.gov.ru/opendata}timeOut" content
  notificationUrl <- tagNoAttr "{http://torgi.gov.ru/opendata}notificationUrl" content
  isFas <- tagNoAttr "{http://torgi.gov.ru/opendata}isFas" content
  startDateRequest <- tagNoAttr "{http://torgi.gov.ru/opendata}startDateRequest" content
  isOnlySmps <- tagNoAttr "{http://torgi.gov.ru/opendata}isOnlySmps" content
  docProvide <- tagNoAttr "{http://torgi.gov.ru/opendata}docProvide" content
  docChargeRateRur <- tagNoAttr "{http://torgi.gov.ru/opendata}docChargeRateRur" content
  expireDate <- tagNoAttr "{http://torgi.gov.ru/opendata}expireDate" content
  appReceiptDetails <- tagNoAttr "{http://torgi.gov.ru/opendata}appReceiptDetails" content
  appRequirement <- tagNoAttr "{http://torgi.gov.ru/opendata}appRequirement" content
  appWithdraw <- tagNoAttr "{http://torgi.gov.ru/opendata}appWithdraw" content
  condition <- tagNoAttr "{http://torgi.gov.ru/opendata}condition" content
  winnerDefineDate <- tagNoAttr "{http://torgi.gov.ru/opendata}winnerDefineDate" content
  winnerDefinePlace <- tagNoAttr "{http://torgi.gov.ru/opendata}winnerDefinePlace" content
  appChange <- tagNoAttr "{http://torgi.gov.ru/opendata}appChange" content
  placeRequest <- tagNoAttr "{http://torgi.gov.ru/opendata}placeRequest" content
  bidAuctionDate <- tagNoAttr "{http://torgi.gov.ru/opendata}bidAuctionDate" content
  bidAuctionPlace <- tagNoAttr "{http://torgi.gov.ru/opendata}bidAuctionPlace" content
  summationDate2 <- tagNoAttr "{http://torgi.gov.ru/opendata}summationDate" content
  summationPlace <- tagNoAttr "{http://torgi.gov.ru/opendata}summationPlace" content
  winnerDefineDescr <- tagNoAttr "{http://torgi.gov.ru/opendata}winnerDefineDescr" content
  bulletinNumber <- tagNoAttr "{http://torgi.gov.ru/opendata}bulletinNumber" content
  processingDate <- tagNoAttr "{http://torgi.gov.ru/opendata}processingDate" content
  summationDate <- tagNoAttr "{http://torgi.gov.ru/opendata}summationDate" content
  return
    $ NDetailsCommon
        bidKind
        bidForm
        bidUrl
        fio
        lotNum
        published
        lastChanged
        timeOut
        notificationUrl
        isFas
        startDateRequest
        isOnlySmps
        docProvide
        docChargeRateRur
        expireDate
        appReceiptDetails
        appRequirement
        appWithdraw
        condition
        winnerDefineDate
        winnerDefinePlace
        appChange
        placeRequest
        bidAuctionDate
        bidAuctionPlace
        summationDate2
        summationPlace
        winnerDefineDescr
        bulletinNumber
        processingDate
        summationDate

parseNDetailsCommonBidForm :: ConduitM Event o (ResourceT IO) NDetailsCommonBidForm
parseNDetailsCommonBidForm = do
  _id <- tagNoAttr "{http://torgi.gov.ru/opendata}id" content
  name <- tagNoAttr "{http://torgi.gov.ru/opendata}name" content
  return
    $ NDetailsCommonBidForm
        _id
        name

parseNDetailsCommonBidKind :: ConduitM Event o (ResourceT IO) NDetailsCommonBidKind
parseNDetailsCommonBidKind = do
  _id <- tagNoAttr "{http://torgi.gov.ru/opendata}id" content
  name <- tagNoAttr "{http://torgi.gov.ru/opendata}name" content
  return
    $ NDetailsCommonBidKind
        _id
        name

parseNDetailsBidOrganization :: ConduitM Event o (ResourceT IO) NDetailsBidOrganization
parseNDetailsBidOrganization = do
  bidOrgKind <- tagNoAttr "{http://torgi.gov.ru/opendata}bidOrgKind" content
  organizationId <- tagNoAttr "{http://torgi.gov.ru/opendata}organizationId" content
  fullName <- tagNoAttr "{http://torgi.gov.ru/opendata}fullName" content
  headOrg <- tagNoAttr "{http://torgi.gov.ru/opendata}headOrg" content
  limitBidDeal <- tagNoAttr "{http://torgi.gov.ru/opendata}limitBidDeal" content
  inn <- tagNoAttr "{http://torgi.gov.ru/opendata}inn" content
  kpp <- tagNoAttr "{http://torgi.gov.ru/opendata}kpp" content
  okato <- tagNoAttr "{http://torgi.gov.ru/opendata}okato" content
  okpo <- tagNoAttr "{http://torgi.gov.ru/opendata}okpo" content
  okved <- tagNoAttr "{http://torgi.gov.ru/opendata}okved" content
  ogrn <- tagNoAttr "{http://torgi.gov.ru/opendata}ogrn" content
  address <- tagNoAttr "{http://torgi.gov.ru/opendata}address" content
  phone <- tagNoAttr "{http://torgi.gov.ru/opendata}phone" content
  fax <- tagNoAttr "{http://torgi.gov.ru/opendata}fax" content
  location <- tagNoAttr "{http://torgi.gov.ru/opendata}location" content
  url <- tagNoAttr "{http://torgi.gov.ru/opendata}url" content
  return
    $ NDetailsBidOrganization
        bidOrgKind
        organizationId
        fullName
        headOrg
        limitBidDeal
        inn
        kpp
        okato
        okpo
        okved
        ogrn
        address
        phone
        fax
        location
        url

parseNotification :: n ~ Notification t => ConduitM Event n (ResourceT IO) (Maybe n)
parseNotification = tagNoAttr "{http://torgi.gov.ru/opendata}notification" $ do
  bidKindId <- tagNoAttr "{http://torgi.gov.ru/opendata}bidKindId" content
  bidKindName <- tagNoAttr "{http://torgi.gov.ru/opendata}bidKindName" content
  bidNumber <- tagNoAttr "{http://torgi.gov.ru/opendata}bidNumber" content
  organizationName <- tagNoAttr "{http://torgi.gov.ru/opendata}organizationName" content
  isArchived <- tagNoAttr "{http://torgi.gov.ru/opendata}isArchived" content
  publishDate <- tagNoAttr "{http://torgi.gov.ru/opendata}publishDate" content
  lastChanged <- tagNoAttr "{http://torgi.gov.ru/opendata}lastChanged" content
  odDetailedHref <- tagNoAttr "{http://torgi.gov.ru/opendata}odDetailedHref" content
  return
    $ Notification
        bidKindId
        bidKindName
        bidNumber
        organizationName
        isArchived
        publishDate
        lastChanged
        odDetailedHref

parseOpenData :: ConduitM Event (Notification t) (ResourceT IO) ()
parseOpenData = force "openData tag missing"
  . tagName "{http://torgi.gov.ru/opendata}openData" ignoreAttrs
  $ \() -> manyYield parseNotification


downloadLotsXml :: LotsUrl t -> LotsFile t -> IO ()
downloadLotsXml (LotsUrl u) (LotsFile f) =
  HN.downloadFileByURL (T.unpack u) (T.unpack f) True

dbConn
 :: (MonadIO m, MonadBaseControl IO m) =>
 SqlPersistT (NoLoggingT (ResourceT m)) a
 -> m a
dbConn =
 runResourceT . runNoLoggingT . withMySQLConn rikardCorpDBci . runSqlConn
