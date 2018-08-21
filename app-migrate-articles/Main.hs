{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import qualified Data.List                     as DL
import           Data.Maybe
import           Data.Text                     as T
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.MySQL
import           Database.Persist.TH
import           RikardCorp.DBConnection
import           RikardCorp.DBTypes
import qualified RikardCorp.Helpers.DateTime   as RHD
import qualified RikardCorp.Helpers.Exceptions as EX
import qualified RikardCorp.Helpers.Html       as HH
import qualified RikardCorp.Helpers.Text       as HT
import           System.Environment
import qualified Text.HTML.TagSoup             as TS
import qualified RikardCorp.Helpers.Internal.Articles as HIA

share [mkPersist sqlSettings] [persistLowerCase|
OldArticles sql=ppa_articles
  date Text sql=article_date
  header Text sql=article_header
  text Text sql=article_text
  lmt UTCTime
  deriving Show
|]

ci :: ConnectInfo
ci = defaultConnectInfo {
  connectHost = "X",
  connectUser = "X",
  connectPassword = "X",
  connectDatabase = "X"
}

main :: IO ()
-- main = migrateArticles
main = putStrLn "Migration complete"

migrateArticles :: IO ()
migrateArticles = do
  args <- getArgs
  oldArticles <- case args of
    i:_ -> getOldArticles (Just . toSqlKey $ fromIntegral (read i :: Int))
    _ -> getOldArticles Nothing
  insertArticles oldArticles

insertArticles :: [Entity OldArticles] -> IO ()
insertArticles _oldArticles = dbConn $ do
  runMigration migrateRikardCorpDB
  forM_ _oldArticles $ \(Entity onkey oarticle) -> do
    let
      tmm :: Maybe UTCTime
      tmm = RHD.dmyToUTC (unpack $ oldArticlesDate oarticle)

      oat :: Text
      oat = preformatOldArticles . oldArticlesText $ oarticle

      oah :: Text
      oah = HH.decodeHtmlEntities . oldArticlesHeader $ oarticle
    case tmm of
      Just tm -> do
        liftIO $ print onkey
        EX.liftedCatchAny
              (insert_ $ Article tm
                oah
                oat
                (Just . HIA.genHtmlArticle $ oat)
                [])
              $ liftIO . print -- show error if exception raised
      _ -> return ()

findOrigin :: Text -> Maybe Text
findOrigin txt =
  let
    originTags :: [[TS.Tag Text]]
    originTags = DL.filter (HH.firstTagTextIs originDefText) $
      TS.sections (TS.isTagOpenName "a") . TS.parseTags $ txt

    originUrl :: Text
    originUrl = if DL.null originTags then empty else TS.fromAttrib "href" $ DL.head . DL.head $ originTags
  in
    if T.null originUrl then Nothing else Just (HH.decodeHtmlEntities $ HT.stripScreening originUrl)

originDefText :: Text
originDefText = "Источник"

cleanArtifacts :: Text -> Text
cleanArtifacts txt =
  let
    tLines :: [Text]
    tLines = T.lines txt

    lLine :: Text
    lLine =
      if (not . DL.null) tLines
        then DL.last tLines
        else T.empty
  in
    if T.null lLine
      then txt
      else
        if lLine == originDefText
          then T.unlines . DL.init $ tLines
          else T.unlines . DL.concat $ [DL.init tLines, [fromMaybe lLine $ T.stripSuffix originDefText lLine]]

preformatOldArticles :: Text -> Text
preformatOldArticles = HT.cleanExcessSpaces . HT.stripScreening . cleanArtifacts . HT.cleanText .
  HH.decodeHtmlEntities . HH.stripTags . HH.brTagToNewLine . HH.pTagToNewLine

getOldArticles :: Maybe (Key OldArticles) -> IO [Entity OldArticles]
getOldArticles _id = dbConnOld $
  case _id of
    Just onkey -> selectList [OldArticlesId >. onkey] [Asc OldArticlesId]
    _ -> selectList [] [Asc OldArticlesId]

dbConn
  :: (MonadIO m, MonadBaseControl IO m) =>
  SqlPersistT (NoLoggingT (ResourceT m)) a
  -> m a
dbConn =
  runResourceT . runNoLoggingT . withMySQLConn rikardCorpDBci . runSqlConn

dbConnOld
  :: (MonadIO m, MonadBaseControl IO m) =>
  SqlPersistT (NoLoggingT (ResourceT m)) a
  -> m a
dbConnOld =
  runResourceT . runNoLoggingT . withMySQLConn ci . runSqlConn
