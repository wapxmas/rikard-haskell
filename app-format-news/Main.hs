-- {-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import qualified Data.List                        as DL
import           Database.Persist
import           Database.Persist.MySQL
import           RikardCorp.DBConnection
import qualified RikardCorp.Helpers.Internal.News as HIN
import qualified RikardCorp.Helpers.NLP.Text      as HNP
import           RikardCorp.Helpers.SqlTags       as HTags
import           RikardCorp.Types
import           System.Directory
import           System.Environment
import           System.FilePath

main :: IO ()
-- main = putStrLn "News formatted"
main = do
  args <- getArgs
  run  <- case args of
    [func, _id] -> return (func, Just $ stringToKey _id)
    [func]      -> return (func, Nothing)
    _           -> usage
  case run of
    ("keywords"  , _id) -> keywordsNews _id
    ("format"    , _id) -> formatNews _id
    ("cleanempty", _id) -> cleanEmptyNews _id
    _                   -> usage

usage :: IO any
usage = error "Usage: .exe <keywords|format|cleanempty> [news id]"

stringToKey :: String -> NewsId
stringToKey _id = toSqlKey $ fromIntegral (read _id :: Int)

cleanEmptyNews :: Maybe NewsId -> IO ()
cleanEmptyNews _ =
  dbConn $ do
    newsList <- selectList ([] :: [Filter News]) []
    forM_ newsList
      $ \(Entity k news) -> when (DL.null (newsTags news)) $ delete k

formatNews :: Maybe NewsId -> IO ()
formatNews nId = case nId of
  Just k ->
    dbConn
      $ do
          Just news <- get k
          update
            k
            [ NewsHtmlBody
              =. Just (HIN.genHtmlNews (newsOrigin news) (newsBody news))
            ]
  _ ->
    dbConn
      $ do
          newsList <- selectList ([] :: [Filter News]) []
          forM_ newsList $ \(Entity k news) -> update
            k
            [ NewsHtmlBody
              =. Just (HIN.genHtmlNews (newsOrigin news) (newsBody news))
            ]

keywordsNews :: Maybe NewsId -> IO ()
keywordsNews nId = do
  let stopWordsPath :: FilePath
      stopWordsPath = "data" </> "stop_words_ru.txt"

      stopKeywordsPath :: FilePath
      stopKeywordsPath = "data" </> "stop_keywords.txt"

  stopWordsPathEx <- doesFileExist stopWordsPath
  unless stopWordsPathEx $ error "stop words file doesn`t exists."

  stopKeywordsPathEx <- doesFileExist stopKeywordsPath
  unless stopKeywordsPathEx $ error "stop keywords file doesn`t exists."

  stopWords    <- HNP.getStopWords stopWordsPath
  stopKeywords <- HNP.getStopWords stopKeywordsPath

  runResourceT
    $ runNoLoggingT
    $ withMySQLConn rikardCorpDBci
    $ runSqlConn
    $ runMigration migrateRikardCorpDB

  tagsMap <-
    runResourceT $ runNoLoggingT $ withMySQLConn rikardCorpDBci $ runSqlConn
      HTags.getTagsMap

  case nId of
    Just newsKey ->
      HIN.updateNewsKeywordsById stopWords stopKeywords tagsMap newsKey
    _ ->
      dbConn
        $ do
            ns <-
              map (\(Entity k _) -> k)
                <$> selectList ([] :: [Filter News]) [Asc NewsId]
            liftIO $ forM_ ns
                           ( HIN.updateNewsKeywordsById stopWords
                                                        stopKeywords
                                                        tagsMap
                           )
  return ()

dbConn
  :: (MonadIO m, MonadBaseControl IO m) =>
  SqlPersistT (NoLoggingT (ResourceT m)) a
  -> m a
dbConn =
  runResourceT . runNoLoggingT . withMySQLConn rikardCorpDBci . runSqlConn
