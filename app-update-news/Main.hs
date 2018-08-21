{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.UTF8             as SU8
import           Data.Char                        (isSpace)
import qualified Data.HashSet                     as S
import           Data.List                        ((\\))
import qualified Data.List                        as DL
import qualified Data.Text                        as T
import           Database.Persist
import           Database.Persist.MySQL
import           RikardCorp.DBConnection
import qualified RikardCorp.Helpers.DateTime      as RHD
import qualified RikardCorp.Helpers.Html          as HH
import qualified RikardCorp.Helpers.Internal.News as HIN
import qualified RikardCorp.Helpers.NLP.Text      as HNT
import qualified RikardCorp.Helpers.NLP.Text      as HNP
import           RikardCorp.Helpers.SqlTags       as HTags
import qualified RikardCorp.Helpers.Text          as HT
import           RikardCorp.Types
import           System.Directory
import           System.FilePath
import           Text.HandsomeSoup
import           Text.Printf
import           Text.XML.HXT.Core
import qualified RikardCorp.Helpers.Networking as HN
import Data.Time.Clock

type NewsUrl = String
type NewsHeader = T.Text
type NewsHeaderWords = [T.Text]
type NewsDate = String

data NewsArticle = NewsArticle {  newsArticleHeader :: T.Text, newsArticleBody :: T.Text,
                                  newsArticleHeaderPrep :: T.Text, newsArticleUrl :: T.Text }

newsUrls :: [String]
newsUrls =
  [
    "http://dgi.mos.ru/presscenter/news/"
  , "http://tender.mos.ru/presscenter/news/"
  , "http://dgi.mos.ru/presscenter/publications/"
  , "http://dgi.mos.ru/presscenter/moscow-news/"
  ]

userAgent :: String
userAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:48.0) Gecko/20100101 Firefox/48.0"

uniThreshold :: Int
uniThreshold = 2

minNewsWords :: Int
minNewsWords = 10

preformatHeader :: NewsHeader -> NewsHeader
preformatHeader = T.toLower . HNT.removeLatinWords' . HT.cleanExcessSpaces . HT.cleanString . HNT.keepOnlyLetters' . HH.stripNBSP . HH.decodeHtmlEntities

preformatBody :: T.Text -> T.Text
preformatBody = preformatHeader

main :: IO ()
main = do
  let
    stopWordsPath :: FilePath
    stopWordsPath = "data" </> "stop_words_ru.txt"

    stopKeywordsPath :: FilePath
    stopKeywordsPath = "data" </> "stop_keywords.txt"

  stopWordsPathEx <- doesFileExist stopWordsPath
  unless stopWordsPathEx $ error "stop words file doesn`t exists."

  stopKeywordsPathEx <- doesFileExist stopKeywordsPath
  unless stopKeywordsPathEx $ error "stop keywords file doesn`t exists."

  stopWords <- HNP.getStopWords stopWordsPath
  stopKeywords <- HNP.getStopWords stopKeywordsPath

  runResourceT $ runNoLoggingT $ withMySQLConn rikardCorpDBci $ runSqlConn $
    runMigration migrateRikardCorpDB

  tagsMap <- runResourceT $ runNoLoggingT $ withMySQLConn rikardCorpDBci $
    runSqlConn HTags.getTagsMap

  today <- RHD.dateString
  -- let today = "18.08.2016"
  dbNewsUrls <- maybe (return S.empty :: IO (S.HashSet NewsUrl)) (\newsDay ->
    dbConn $
      S.fromList . map (\(Entity _ n) -> T.unpack . newsMonitorUrl $ n) <$>
        selectList [NewsMonitorDate ==. newsDay] [Asc NewsMonitorId]) (RHD.dmyToUTC today)

  dbNewsHeaders <- map (T.words . preformatHeader) <$> maybe (return [] :: IO [NewsHeader]) (\newsDay ->
    dbConn $
      map (\(Entity _ n) -> newsHeader n) <$> selectList
        [NewsDate ==. newsDay] [Asc NewsId]) (RHD.dmyToUTC today)

  newsXmlData <- map (uncurry extractNewsUrls . second (readString [withParseHTML yes, withWarnings no,
    withInputEncoding utf8]) . second SU8.toString) <$> traverse (HN.getContentsByURL userAgent) newsUrls

  news <- filter (\(d, u) -> d == today && not (S.member u
    dbNewsUrls)) . concat <$> traverse runX newsXmlData

  forM_ news print
  monUrls <- getMonitorNewsUrls
  newsContent <- (filterNewsByWordsLength . filterNewsByExistsUrls monUrls)
              <$> mapM downloadNews news
  newsMonitorTm <- getNewsMonitorTm
  nids <- addNews newsMonitorTm newsContent dbNewsHeaders
  forM_ nids (HIN.updateNewsKeywordsById stopWords stopKeywords tagsMap)
  return ()

getMonitorNewsUrls :: IO (S.HashSet T.Text)
getMonitorNewsUrls = do
  urls <- map (newsMonitorUrl . entityVal)
      <$> dbConn (selectList ([] :: [Filter NewsMonitor]) [])
  return $ S.fromList urls

filterNewsByExistsUrls :: S.HashSet T.Text -> [(NewsDate, Maybe NewsArticle)] -> [(NewsDate, Maybe NewsArticle)]
filterNewsByExistsUrls set (ne@(_, Just n):ns) =
  if S.member (newsArticleUrl n) set
    then filterNewsByExistsUrls set ns
    else ne : filterNewsByExistsUrls set ns
filterNewsByExistsUrls set (n:ns) = n : filterNewsByExistsUrls set ns
filterNewsByExistsUrls _ [] = []

filterNewsByWordsLength :: [(NewsDate, Maybe NewsArticle)] -> [(NewsDate, Maybe NewsArticle)]
filterNewsByWordsLength (ne@(_, Just n):ns) =
  if (DL.length . T.words . preformatBody . newsArticleBody $ n) >= minNewsWords
    then ne : filterNewsByWordsLength ns
    else filterNewsByWordsLength ns
filterNewsByWordsLength (n:ns) = n : filterNewsByWordsLength ns
filterNewsByWordsLength [] = []

addNews :: UTCTime -> [(NewsDate, Maybe NewsArticle)] -> [NewsHeaderWords] -> IO [NewsId]
addNews mtm ((nd, Just newsArticle):ns) headers = do
  res <- dbConn $ do
    let
      aheader :: T.Text
      aheader = newsArticleHeader newsArticle

      abody :: T.Text
      abody = newsArticleBody newsArticle

      alink :: T.Text
      alink = newsArticleUrl newsArticle

      prepHeader :: NewsHeaderWords
      prepHeader = T.words . newsArticleHeaderPrep $ newsArticle

      uniVal :: Int
      uniVal = foldl (\v h -> min v . length . (\\) prepHeader $ h) (uniThreshold + 1) headers

      headerEx :: Bool
      headerEx = prepHeader `elem` headers

    if ((length prepHeader <= 3) && not headerEx) || uniVal > uniThreshold
      then
        case RHD.dmyToUTC nd of
          Just tm -> do
            liftIO $ putStrLn $ "Insert: " ++ T.unpack alink
            k <- insert $ News tm (T.toUpper aheader) abody
              (Just (HIN.genHtmlNews (Just alink) abody))
              (Just alink) []
            insert_ $ NewsMonitor alink NotPosted (Just k) mtm
            return (Just (k, prepHeader : headers))
          _ -> return Nothing
      else do
        liftIO $ putStrLn $ printf "News exists (uniV: %d): %s" uniVal (T.unpack alink)
        return Nothing
  case res of
    Just (nid, hdrs) -> (:) nid <$> addNews mtm ns hdrs
    _ -> addNews mtm ns headers
addNews mtm ((_, _):ns) headers = addNews mtm ns headers
addNews _ _ _ = return []

getNewsMonitorTm :: IO UTCTime
getNewsMonitorTm = do
  mtm <- dbConn $ do
    nms <- selectList [NewsMonitorPostStatus ==. NotPosted] [Desc NewsMonitorDate]
    return $ case nms of
      Entity _ v : _ -> Just . newsMonitorDate $ v
      _ -> Nothing
  maybe getCurrentTime return mtm

downloadNews :: (NewsDate, NewsUrl) -> IO (NewsDate, Maybe NewsArticle)
downloadNews (nd, newsUrl) = do
  (_, newsContent) <- HN.getContentsByURL userAgent newsUrl
  let
    doc :: IOSArrow XmlTree XmlTree
    doc = readString [withParseHTML yes, withWarnings no, withInputEncoding utf8] (SU8.toString newsContent)
  (,) nd <$> (fmap fst . DL.uncons <$> runX (extractNewsArticle newsUrl doc))

extractNewsArticle :: NewsUrl -> IOSArrow XmlTree XmlTree -> IOSArrow XmlTree NewsArticle
extractNewsArticle nurl doc = doc >>> css "div[class~=primary_content]" >>>
  proc newsArticle -> do
    header <- deep (hasName "h1") /> getText >>^ trimString -< newsArticle
    content <- deep (hasName "article") >>> writeDocumentToString [] -< newsArticle
    let
      textHeader :: NewsHeader
      textHeader = HH.decodeHtmlEntities . T.pack $ header

      newsContent :: T.Text
      newsContent = HT.cleanExcessSpaces . HT.cleanText . HH.decodeHtmlEntities . preformatRawNews . T.pack $ content

    returnA -< NewsArticle textHeader newsContent (preformatHeader textHeader) (T.pack nurl)

preformatRawNews :: T.Text -> T.Text
preformatRawNews = HH.stripTags . HH.pTagToNewLine

extractNewsUrls :: HN.AbsoluteURL -> IOSArrow XmlTree XmlTree -> IOSArrow XmlTree (String, String)
extractNewsUrls url doc = doc >>> css "ul[class~=newslist]" >>> deep (hasName "li") >>> deep (hasName "p") >>>
  proc newsElement -> do
    nDate <- css "span[class~=date]" /> getText >>^ trimString -< newsElement
    nUrl  <- deep (hasName "a") >>> neg (hasAttrValue "class" (== "rubr")) >>> getAttrValue "href" >>^ (url ++) -< newsElement
    returnA -< (nDate, nUrl)

trimString :: String -> String
trimString = f . f
   where f = reverse . dropWhile isSpace

dbConn
 :: (MonadIO m, MonadBaseControl IO m) =>
 SqlPersistT (NoLoggingT (ResourceT m)) a
 -> m a
dbConn =
 runResourceT . runNoLoggingT . withMySQLConn rikardCorpDBci . runSqlConn
