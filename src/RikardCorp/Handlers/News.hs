{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module RikardCorp.Handlers.News where

  import           Control.Arrow
  import           Control.Monad
  import           Data.Function
  import qualified Data.HashMap.Lazy           as M
  import qualified Data.List                   as DL
  import           Data.Maybe
  import qualified Data.Text                   as T
  import           Data.Time.Clock
  import           Database.Persist.MySQL
  import           RikardCorp.Foundation       ()
  import qualified RikardCorp.Helpers.DateTime as HDT
  import qualified RikardCorp.Helpers.Sql      as HS
  import qualified RikardCorp.Helpers.SqlTags      as HST
  import qualified RikardCorp.Helpers.Text     as HT
  import           RikardCorp.Static
  import           RikardCorp.Types
  import qualified RikardCorp.Widgets          as W
  import           Yesod

  type PageTitle = T.Text
  type TitleWidget = Widget
  type TotalSize = Int

  getNewsR :: Handler Html
  getNewsR = getNewsIndexR upageNothing

  getNewsIndexR :: UPage -> Handler Html
  getNewsIndexR upg = do
    ns <- map (entityKey &&& entityVal) <$> runDB (selectList ([] :: [Filter News]) $
      setPageOpts defNewsOnPage (getPageNum upg) ++ [Desc NewsId, LimitTo defNewsOnPage])
    let
      news :: [[(Key News, News)]]
      news = DL.groupBy ((==) `on` (newsDate . snd)) ns

    newsIndexUniHandler "Новости торгов" Nothing news (length ns) upg NewsIndexR (Just NewsR)

  getNewsTagsIndexR :: TagId -> UPage -> Handler Html
  getNewsTagsIndexR tid upg = do
    tagentity <- runDB $ get404 tid
    ns <- runDB $ do
      ntis <- selectList
        [NewsTagIndexTag ==. tid]
        (setPageOpts defNewsOnPage (getPageNum upg) ++ [Desc NewsTagIndexNews])
      let
        nks :: [NewsId]
        nks = map (newsTagIndexNews . entityVal) ntis
      map (\(k, Just v) -> (k, v)) . filter (\(_, v)
        -> isJust v) . zip nks <$> forM nks get
    let
      news :: [[(Key News, News)]]
      news = DL.groupBy ((==) `on` (newsDate . snd)) . DL.take defNewsOnPage $ ns

    newsIndexUniHandler ("Новости торгов с тегом #" `T.append` tagName tagentity) (Just (newsTagsTitleWidget
      tagentity)) news (length ns) upg (NewsTagsIndexR tid) (Just (NewsTagsIndexR tid upageNothing))

  getNewsTextR :: NewsId -> Handler Html
  getNewsTextR nid = do
    news <- runDB $ get404 nid
    mtags <- getMasterTagsMap <$> getYesod
    tags <- flip (maybe (error "No tags map specified")) mtags $ \tagsMap ->
      runDB $ do
        let
          tg :: [Int]
          tg = map (fromIntegral . fromSqlKey) . newsTags $ news
        if not . null $ tg
          then M.fromList . map (second fromJust) . DL.filter (\(_, j) ->
            isJust j) . zip tg <$> forM tg (HST.getTagById tagsMap)
          else return (M.empty :: M.HashMap Int Tag)
    defaultLayout $ do
      setTitle $ toHtml . HT.capitalString . newsHeader $ news
      W.descWidget (T.unwords . DL.init . T.words . HT.cleanExcessSpaces .
        HT.cleanString . HT.hideCRLF . T.take maxShortTextSize . newsBody $ news)
      W.keyWidget (T.intercalate ", " . map tagName . M.elems $ tags)
      let
        html :: Html
        html = maybe (toHtml . newsBody $ news) preEscapedToMarkup (newsHtmlBody news)

        tm :: UTCTime
        tm = newsDate news
      [whamlet|
      <article itemscope itemtype="http://schema.org/Article">
        <meta itemprop="inLanguage" content="ru-RU">
        <meta itemprop="image" content="@{StaticR img_logo_png}">
        <h4 class="text-center" itemprop="headline">
          <strong>#{newsHeader news}
        <div class="row">
          <div class="col-lg-10 col-lg-offset-1 col-md-10 col-md-offset-1 col-sm-10 col-sm-offset-1 col-xs-12 margin-top-20">
            <div class="row">
              <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
                <time datetime=#{HDT.iso8601 tm} itemprop="datePublished">
                  <strong>#{HDT.fmtTimeToHumanDay tm}
                <div class="margin-top-20" itemprop="articleBody">
                  #{html}
                <div .margin-top-20 .text-center>
                  $forall tag <- newsTags news
                    $maybe tagentity <- M.lookup (HS.sqlKeyInt tag) tags
                      <a .label .label-default style="font-size:12px" href=@{NewsTagsIndexR tag upageNothing}>##{tagName tagentity}
      |]

  defNewsOnPage :: ElementsOnPage
  defNewsOnPage = 10

  maxShortTextSize :: Int
  maxShortTextSize = 255

  newsTagsTitleWidget :: Tag -> Widget
  newsTagsTitleWidget tagentity =
    [whamlet|
    <h1 .text-center>
      Новости торгов с тегом #
      <span .label .label-default>##{tagName tagentity}
    |]

  newsIndexUniHandler :: PageTitle -> Maybe TitleWidget -> [[(Key News, News)]] -> TotalSize ->
    UPage -> (UPage -> Route RikardCorp) -> Maybe (Route RikardCorp) -> Handler Html
  newsIndexUniHandler ttl ttw news nsSize upg mkRoute route =
    defaultLayout $ do
      setTitle $ toHtml ttl
      [whamlet|
      <section>
        $maybe w <- ttw
          ^{w}
        $nothing
          <h1 .text-center>Новости торгов
        <div .row>
          <div .col-lg-10 .col-lg-offset-1 .col-md-10 .col-md-offset-1 .col-sm-10 .col-sm-offset-1 .col-xs-12>
            <div .row>
              <div .col-lg-12 .col-md-12 .col-sm-12 .col-xs-12>
                $if not (DL.null news)
                  <ul .listSection .list-unstyled>
                    $forall newsgr <- news
                      <li>
                        $with (_, n) <- DL.head newsgr
                          $with tm <- newsDate n
                            <time datetime="#{HDT.iso8601 tm}">#{HDT.fmtTimeToHumanDay tm}
                      $forall (k, n) <- newsgr
                        <li>
                          <section>
                            <h4 .listHeader>
                              <a href=@{NewsTextR k}>#{newsHeader n}
                            <p .hidden-xs .text-justify>
                              #{T.take maxShortTextSize (newsBody n)} #
                              <a href=@{NewsTextR k} rel=nofollow>Читать далее...
                  ^{W.paggingWidget nsSize upg defNewsOnPage mkRoute route}
      |]
