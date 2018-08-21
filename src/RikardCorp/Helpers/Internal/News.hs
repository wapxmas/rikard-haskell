{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module RikardCorp.Helpers.Internal.News where

  import           Control.Monad
  import           Control.Monad.IO.Class       (liftIO)
  import           Control.Monad.Logger
  import           Control.Monad.Trans.Resource
  import qualified Data.HashSet                 as S
  import qualified Data.Text                    as T
  import qualified Data.Text.Lazy as TL
  import           Database.Persist
  import           Database.Persist.MySQL
  import qualified NLP.Mystem                   as MSTEM
  import           RikardCorp.DBConnection
  import           RikardCorp.Types
  import qualified RikardCorp.Helpers.NLP.Text  as HNP
  import RikardCorp.Helpers.SqlTags as HTags
  import           Text.Printf
  import qualified RikardCorp.Helpers.Text.Textile as HTT
  import Text.Blaze.Html.Renderer.Text (renderHtml)

  type NewsLink = T.Text
  type NewsBody = T.Text

  genHtmlNews :: Maybe NewsLink -> NewsBody -> T.Text
  genHtmlNews link = TL.toStrict . renderHtml . HTT.parsePreformattedText . HTT.preformatText
    exceedNewsTexts (appendLink link) deletionNewsTexts

  appendLink :: Maybe T.Text -> T.Text
  appendLink = maybe T.empty (HTT.originLink "Источник")

  updateNewsKeywordsById :: HNP.StopWordsList -> HNP.StopWordsList -> TagsMap -> NewsId -> IO ()
  updateNewsKeywordsById stopWords stopKeywords tagsMap newsKey =
    runResourceT $ runNoLoggingT $ withMySQLConn rikardCorpDBci $ runSqlConn $ do
      Just news <- get newsKey
      when (null (newsTags news)) $ do
        ks <- liftIO $ do
          let
            strings :: [T.Text]
            strings = HNP.mkKeywordsLitStrings exceedNewsTexts stopWords $ newsBody news

            ws :: HNP.WordsStat
            ws = HNP.wordsStat strings

          ksRaw <- flip zip ws <$> MSTEM.getStems (HNP.getOriginalWords ws)
          when (length ksRaw /= length ws) $ error "Word stems incorrect size"
          let
            newsKeywords :: [T.Text]
            newsKeywords = HNP.mkStdKeywords stopKeywords ksRaw
          printf "(%d): %d keys\n" (fromSqlKey newsKey) (length newsKeywords)
          return newsKeywords

        unless (null ks) $ do
          tagsIds <- traverse (HTags.getTagId tagsMap) ks
          update newsKey [NewsTags =. tagsIds]
          forM_ tagsIds (insertUnique . flip NewsTagIndex newsKey)
          return ()

  exceedNewsTexts :: S.HashSet T.Text
  exceedNewsTexts = S.fromList ["всегда на связи!", "всегда на связи", "отдел по связям с общественностью и сми"]

  deletionNewsTexts :: [T.Text]
  deletionNewsTexts = ["Документация"]
