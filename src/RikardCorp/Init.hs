{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module RikardCorp.Init where

  import           Control.Monad.Logger
  import           Control.Monad.Trans.Resource
  import           Data.Default
  import qualified Data.HashTable.IO             as HT
  import           Database.Persist.MySQL
  import           RikardCorp.DBConnection
  import           RikardCorp.Foundation         ()
  import           RikardCorp.Handlers.Additions
  import           RikardCorp.Handlers.Articles
  import           RikardCorp.Handlers.Contacts
  import           RikardCorp.Handlers.Favicon
  import           RikardCorp.Handlers.Fraud
  import           RikardCorp.Handlers.Index
  import           RikardCorp.Handlers.Letter
  import           RikardCorp.Handlers.Main
  import           RikardCorp.Handlers.News
  import           RikardCorp.Handlers.Robots
  import qualified RikardCorp.Helpers.SqlTags    as HST
  import           RikardCorp.Types
  import           Text.Printf
  import           Yesod

  mkYesodDispatch "RikardCorp" resourcesRikardCorp

  app :: RikardCorp
  app = def

  connectionsCount :: Int
  connectionsCount = 10

  mkWaiApplication :: IO Application
  mkWaiApplication = runResourceT $ runNoLoggingT $ withMySQLPool rikardCorpDBci connectionsCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ runMigration migrateRikardCorpDB
    tags <- runResourceT $ flip runSqlPool pool $ do
      r <- HST.getTagsIdsMap
      liftIO $ do
        size <- HT.foldM (\v _ -> return (v + 1)) (0 :: Int) r
        putStrLn $ printf "Loaded tags: %d" size
      return r
    toWaiApp app { connectionsPool = Just pool, getMasterTagsMap = Just tags }
