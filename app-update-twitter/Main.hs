{-# LANGUAGE OverloadedStrings #-}

module Main where

  import           Control.Exception
  import           Control.Lens                   hiding ((<.))
  import           Control.Monad
  import           Control.Monad.Logger
  import           Control.Monad.Trans.Resource
  import qualified Data.List                      as DL
  import           Data.Maybe
  import qualified Data.Text                      as T
  import qualified Data.Text.IO                   as TIO
  import           Data.Time.Clock
  import           Database.Persist
  import           Database.Persist.MySQL
  import           Network.Connection
  import           Network.HTTP.Client.TLS
  import           RikardCorp.DBConnection
  import qualified RikardCorp.Helpers.Networking  as HN
  import qualified RikardCorp.Helpers.Sql         as HS
  import qualified RikardCorp.Helpers.SqlTags     as HST
  import qualified RikardCorp.Helpers.Text        as HT
  import           RikardCorp.Types
  import           System.Random
  import qualified Web.Twitter.Conduit            as TW
  import qualified Web.Twitter.Conduit.Parameters as TWP
  import qualified Web.Twitter.Types.Lens         as TWL
  import           Yesod
  import System.IO.Temp
  import qualified Data.ByteString                  as B
  import System.IO

  maxNewsTextSize :: Int
  maxNewsTextSize = 255

  userAgent :: String
  userAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:48.0) Gecko/20100101 Firefox/48.0"

  main :: IO ()
  main = do
    tm <- getCurrentTime
    rnd <- getStdGen
    runResourceT $ runNoLoggingT $ withMySQLConn rikardCorpDBci $ runSqlConn $ do
      runMigration migrateRikardCorpDB
      newsList <- filter (\(Entity _ newsMon) -> isJust (newsMonitorNid newsMon)) <$>
        selectList [NewsMonitorPostStatus ==. NotPosted, NewsMonitorDate <. tm] [Asc NewsMonitorId]
      unless (DL.null newsList) $ do
        let
          newsMon :: NewsMonitor
          (Entity keyMon newsMon) = head newsList

          newsNid :: NewsId
          (Just newsNid) = newsMonitorNid newsMon

        mnews <- get newsNid
        when (isJust mnews) $ do
          tagsMap <- HST.getTagsIdsMap
          let
            Just news = mnews
            txtHeader = newsHeader news
            txtBody = HT.cleanCRLF . HT.cleanExcessSpaces $ T.unwords . flip (++) ["..."] . DL.init . T.words . T.take maxNewsTextSize $ newsBody news
            txtLink = HN.renderYesodRoute "https://rikard.ru" (NewsTextR newsNid)
            tagIds = newsTags news
          tags <- map (\(Just t) -> ('#' `T.cons`) . tagName $ t) . DL.filter isJust <$>
            forM tagIds (HST.getTagById tagsMap . HS.sqlKeyInt)
          let
            hashTags = T.unwords tags
            -- tweets that contain URLs will be reduced to 118 characters, 117 for https links
            tweet = HT.cleanString $ takeStripWord 117 (T.unwords [txtLink, hashTags])
          liftIO $
            withSystemTempFile "tweet" $ \fp fh -> do
              (_, pngData) <- HN.getContentsByURL userAgent $ T.unpack (HN.renderUrl "http://img.rikard.ru"
                ["text2png", "image.php"] [("header", txtHeader), ("text", txtBody)])
              B.hPut fh pngData >> hClose fh
              print tm
              handle (\e -> TIO.putStrLn tweet >> print (e :: SomeException)) $ sendTweetWithMedia fp tweet
              putStrLn $ "#" ++ show (HS.sqlKeyInt newsNid)
          update keyMon [NewsMonitorPostStatus =. Posted]
          return ()
      let
        newsRest :: [Entity NewsMonitor]
        newsRest = DL.drop 1 newsList
      unless (DL.null newsRest) $ do
        let
          addition :: Integer
          (addition, _) = randomR (1800, 7200) rnd
          newTime :: UTCTime
          newTime = addUTCTime (fromInteger addition) tm
        forM_ newsRest $ \(Entity k _) ->
          update k [NewsMonitorDate =. newTime]
        return ()

  takeStripWord :: Int -> T.Text -> T.Text
  takeStripWord maxLen txt =
    if T.length txt > maxLen
      then T.unwords . DL.init . T.words . T.take maxLen $ txt
      else txt

  sendTweetWithMedia :: FilePath -> T.Text -> IO ()
  sendTweetWithMedia file tweet = do
    let
      twInfo :: TW.TWInfo
      twInfo = getTWInfo
    mgr <- TW.newManager (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
    rMedia <- TW.call twInfo mgr $ TW.mediaUpload (TW.MediaFromFile file)
    void $ TW.call twInfo mgr $ TW.update tweet & TWP.mediaIds ?~ ([rMedia] ^.. traversed . TWL.uploadedMediaId)

  sendTweet :: T.Text -> IO ()
  sendTweet tweet = do
    let
      twInfo :: TW.TWInfo
      twInfo = getTWInfo
    mgr <- TW.newManager (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
    void $ TW.call twInfo mgr $ TW.update tweet

  getTWInfo :: TW.TWInfo
  getTWInfo =
    let
      oauth = TW.twitterOAuth { TW.oauthConsumerKey = "X"
                           , TW.oauthConsumerSecret = "X"
                           }
      cred = TW.Credential [ ("oauth_token", "X")
                        , ("oauth_token_secret", "X")
                        ]
    in (TW.setCredential oauth cred TW.def) { TW.twProxy = Nothing }
