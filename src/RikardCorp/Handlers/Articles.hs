{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module RikardCorp.Handlers.Articles where

  import           Control.Arrow
  import           Data.Function
  import qualified Data.List                   as DL
  import qualified Data.Text                   as T
  import           Data.Time.Clock
  import           Database.Persist.MySQL
  import           RikardCorp.Foundation       ()
  import qualified RikardCorp.Helpers.DateTime as HDT
  import qualified RikardCorp.Helpers.Text     as HT
  import           RikardCorp.Static
  import           RikardCorp.Types
  import qualified RikardCorp.Widgets          as W
  import           Yesod

  type PageTitle = T.Text
  type TitleWidget = Widget
  type TotalSize = Int

  defArticlesOnPage :: ElementsOnPage
  defArticlesOnPage = 10

  maxShortTextSize :: Int
  maxShortTextSize = 255

  getArticleTextR :: ArticleId -> Handler Html
  getArticleTextR aid = do
    article <- runDB $ get404 aid
    defaultLayout $ do
      setTitle $ toHtml . HT.capitalString . articleHeader $ article
      W.descWidget (T.unwords . DL.init . T.words . HT.cleanExcessSpaces .
        HT.cleanString . HT.hideCRLF . T.take maxShortTextSize . articleBody $ article)
      let
        html :: Html
        html = maybe (toHtml . articleBody $ article) preEscapedToMarkup (articleHtmlBody article)

        tm :: UTCTime
        tm = articleDate article
      [whamlet|
      <article itemscope itemtype="http://schema.org/Article">
        <meta itemprop="inLanguage" content="ru-RU">
        <meta itemprop="image" content="@{StaticR img_logo_png}">
        <h4 class="text-center" itemprop="headline">
          <strong>#{articleHeader article}
        <div class="row">
          <div class="col-lg-10 col-lg-offset-1 col-md-10 col-md-offset-1 col-sm-10 col-sm-offset-1 col-xs-12 margin-top-20">
            <div class="row">
              <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
                <time datetime=#{HDT.iso8601 tm} itemprop="datePublished">
                  <strong>#{HDT.fmtTimeToHumanDay tm}
                <div class="margin-top-20" itemprop="articleBody">
                  #{html}
      |]

  getArticlesR :: Handler Html
  getArticlesR = getArticlesIndexR upageNothing

  getArticlesIndexR :: UPage -> Handler Html
  getArticlesIndexR upg = do
    as <- map (entityKey &&& entityVal) <$> runDB (selectList ([] :: [Filter Article]) $
      setPageOpts defArticlesOnPage (getPageNum upg) ++ [Desc ArticleId, LimitTo defArticlesOnPage])
    let
      articles :: [[(ArticleId, Article)]]
      articles = DL.groupBy ((==) `on` (articleDate . snd)) as

    articlesIndexUniHandler "Статьи" Nothing articles (length as) upg ArticlesIndexR (Just ArticlesR)

  articlesIndexUniHandler :: PageTitle -> Maybe TitleWidget -> [[(ArticleId, Article)]] -> TotalSize ->
    UPage -> (UPage -> Route RikardCorp) -> Maybe (Route RikardCorp) -> Handler Html
  articlesIndexUniHandler ttl ttw articles nsSize upg mkRoute route =
    defaultLayout $ do
      setTitle $ toHtml ttl
      [whamlet|
      <section>
        $maybe w <- ttw
          ^{w}
        $nothing
          <h1 .text-center>#{ttl}
        <div .row>
          <div .col-lg-10 .col-lg-offset-1 .col-md-10 .col-md-offset-1 .col-sm-10 .col-sm-offset-1 .col-xs-12>
            <div .row>
              <div .col-lg-12 .col-md-12 .col-sm-12 .col-xs-12>
                $if not (DL.null articles)
                  <ul .listSection .list-unstyled>
                    $forall articlegr <- articles
                      <li>
                        $with (_, n) <- DL.head articlegr
                          $with tm <- articleDate n
                            <time datetime="#{HDT.iso8601 tm}">#{HDT.fmtTimeToHumanDay tm}
                      $forall (k, n) <- articlegr
                        <li>
                          <section>
                            <h4 .listHeader>
                              <a href="@{ArticleTextR k}">#{articleHeader n}
                            <p .hidden-xs .text-justify>
                              #{T.take maxShortTextSize (articleBody n)} #
                              <a href="@{ArticleTextR k}" rel=nofollow>Читать далее...
                  ^{W.paggingWidget nsSize upg defArticlesOnPage mkRoute route}
      |]
