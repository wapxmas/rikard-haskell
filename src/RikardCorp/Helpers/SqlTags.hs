{-# LANGUAGE RankNTypes #-}

module RikardCorp.Helpers.SqlTags where

  import           Control.Monad.IO.Class
  import qualified Data.HashTable.IO      as HT
  import qualified Data.Text              as T
  import           Database.Persist
  import           Database.Persist.MySQL
  import           RikardCorp.Types
  import           RikardCorp.Helpers.Sql

  getTagsIdsMap :: SqlM TagsIdsMap
  getTagsIdsMap = do
    tags <- HT.fromList . map (\(Entity key tag) -> (sqlKeyInt key, tag)) <$> selectList ([] :: [Filter Tag]) []
    liftIO tags

  getTagById :: TagsIdsMap -> Int -> SqlM (Maybe Tag)
  getTagById tagsMap tagId = do
    mtag <- liftIO $ HT.lookup tagsMap tagId
    case mtag of
      Just tag -> return $ Just tag
      _ -> do
        let
          tagKey :: TagId
          tagKey = toSqlKey . fromIntegral $ tagId
        mtag' <- get tagKey
        case mtag' of
          Just tag' -> do
            liftIO $ HT.insert tagsMap tagId tag'
            return $ Just tag'
          _ -> return Nothing

  getTagsMap :: SqlM TagsMap
  getTagsMap = do
    tags <- HT.fromList . map (\(Entity key tag) -> (tagName tag, key)) <$> selectList ([] :: [Filter Tag]) []
    liftIO tags

  getTagId :: TagsMap -> T.Text -> SqlM TagId
  getTagId tagsMap tag = do
    key <- liftIO $ HT.lookup tagsMap tag
    case key of
      Just k -> return k
      _ -> do
        newKey <- getTagId' tag
        liftIO $ HT.insert tagsMap tag newKey
        return newKey

  getTagId' :: T.Text -> SqlM TagId
  getTagId' tag = do
    mk <- insertUnique (Tag tag)
    case mk of
      Just key -> return key
      _ -> do
        me <- getBy (UniqueTagName tag)
        case me of
          Just (Entity key _) -> return key
          _ -> error "getTagId': Impossible: Cannot find key."
