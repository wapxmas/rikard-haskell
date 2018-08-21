{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RikardCorp.Widgets where

  import           Control.Arrow
  import           Control.Monad
  import           Data.Function
  import qualified Data.HashMap.Lazy           as M
  import qualified Data.HashSet                as S
  import           Data.List                   as DL
  import           Data.Maybe
  import qualified Data.Text                   as T
  import           Data.Time.Clock
  import           Database.Persist.MySQL
  import           RikardCorp.Foundation       ()
  import qualified RikardCorp.Helpers.DateTime as HDT
  import qualified RikardCorp.Helpers.Sql      as HS
  import qualified RikardCorp.Helpers.SqlTags  as HST
  import           RikardCorp.Types
  import           Yesod

  maybeTextField :: Maybe a -> (a -> Maybe T.Text) -> T.Text
  maybeTextField (Just v) f = fromMaybe T.empty (f v)
  maybeTextField _ _ = T.empty

  successJumbo :: T.Text -> T.Text -> Widget
  successJumbo header body = [whamlet|
  <section class="jumbotron">
    <h2>#{header}
    <p>#{body}
    <p>
      <a class="btn btn-primary btn-lg" href="@{IndexR}" role="button">Перейти на главную
  |]

  errorMessageWidget :: T.Text -> Widget
  errorMessageWidget message = [whamlet|
  <div class="row">
    <div class="col-lg-8 col-lg-offset-2 col-md-8 col-md-offset-2 col-sm-8 col-sm-offset-2 col-xs-10 col-xs-offset-1">
      <div class="alert alert-danger" role="alert">#{message}
  |]

  defNewsOnWidget :: ElementsOnPage
  defNewsOnWidget = 10

  newsIndexWidget :: Widget
  newsIndexWidget = do
    news <- handlerToWidget . runDB $
        head . groupBy ((==) `on` (newsDate . snd)) . map (entityKey &&& entityVal)
          <$> selectList [] [Desc NewsId, LimitTo defNewsOnWidget]
    unless (null news) $ do
      mtags <- getMasterTagsMap <$> getYesod
      tags <- flip (maybe (error "No tags map specified")) mtags $ \tagsMap ->
        handlerToWidget . runDB $ do
          let
            tg :: [Int]
            tg = S.toList . S.fromList . concatMap (map
              (fromIntegral . fromSqlKey) . newsTags . snd) $ news
          if not . null $ tg
            then M.fromList . map (second fromJust) . DL.filter (\(_, j) ->
              isJust j) . zip tg <$> forM tg (HST.getTagById tagsMap)
            else return (M.empty :: M.HashMap Int Tag)
      let
        headNewsDate :: UTCTime
        headNewsDate = newsDate . snd . head $ news
      isCurrent <- handlerToWidget . liftIO $ HDT.isCurrentDay headNewsDate
      let
        today :: String
        today =
          if isCurrent
            then "сегодня"
            else HDT.fmtTimeToHumanDay headNewsDate
      [whamlet|
      <section .col-md-12>
        <div .panel .panel-default>
          <header .panel-heading .header-green>
            <h4 .hx-bold style="font-size:1.2em">Новости за #{today}
          <div .panel-body>
            <ul .list-unstyled .articles-list-short>
              $forall (k, n) <- news
                $with tm <- newsDate n
                  <li>
                    <section>
                      <h5 style="font-size:1.05em">
                        <a href="@{NewsTextR k}">#{newsHeader n}
                      <div .margin-top-5 .visible-lg .visible-md>
                        $forall tag <- newsTags n
                          $maybe tagentity <- M.lookup (HS.sqlKeyInt tag) tags
                            <a .label style="background-color:#999; font-size:12px" href=@{NewsTagsIndexR tag upageNothing}>##{tagName tagentity}
                      <small>
                        <time datetime="#{HDT.iso8601 tm}">#{HDT.fmtTimeToHumanDay tm}
      |]

  descWidget :: T.Text -> Widget
  descWidget txt =
    toWidgetHead [hamlet|
    <meta name="description" content="#{txt}">
    |]

  keyWidget :: T.Text -> Widget
  keyWidget keys =
    toWidgetHead [hamlet|
    <meta name="keywords" content="#{keys}">
    |]

  paggingWidget ::
    forall p route mkRoute elms.
    (p ~ UPage, route ~ (Route RikardCorp), mkRoute ~ (p -> route), elms ~ ElementsOnPage) =>
    elms -> p -> elms -> mkRoute -> Maybe route -> Widget
  paggingWidget elms pg elmsDef mkRt fpRt = do
    let
      pageNum :: Int
      pageNum = getPageNum pg

      nextPageNeed :: Bool
      nextPageNeed = elms > elmsDef

      pageBlockNeed :: Bool
      pageBlockNeed = nextPageNeed || pageNum > 1

    urlRenderer <- handlerToWidget getUrlRender

    let
      prevBtn :: T.Text
      prevBtn =
        if pageNum > 1
          then urlRenderer $
            let
              pn :: Int
              pn = pageNum - 1

              rt :: route
              rt = mkRt . UPage . Just $ pn
            in
              if pn > 1
                then rt
                else
                  case fpRt of
                    Just fp -> fp
                    _ -> rt
          else T.empty

      nextBtn :: T.Text
      nextBtn =
        if nextPageNeed
          then urlRenderer (mkRt . UPage . Just $ pageNum + 1)
          else T.empty

    toWidgetHead [hamlet|
    $if not (T.null prevBtn)
      <link rel="prev" href="#{prevBtn}">
    $if not (T.null nextBtn)
      <link rel="next" href="#{nextBtn}">
    |]

    [whamlet|
    $if pageBlockNeed
      <nav .text-center .hidden-print>
        <ul .pagination>
          <li>
            <a :not (T.null prevBtn):href=#{prevBtn}>
              <span aria-hidden="true" class="text-uppercase">&larr;
          $if nextPageNeed
            <li .active>
              <a>#{pageNum}
            <li>
              <a href=#{nextBtn}>#{pageNum + 1}
          $else
            <li>
              <a href=#{prevBtn}>#{pageNum - 1}
            <li .active>
              <a>#{pageNum}
          <li>
            <a :not (T.null nextBtn):href=#{nextBtn}>
              <span aria-hidden="true" class="text-uppercase">&rarr;
    |]

  mainTabsWidget :: Widget
  mainTabsWidget = do
    let sections1 :: [Section]
        sections1 = take 2 sectionsList

        sections2 :: [Section]
        sections2 = [head $ drop (length sections1) sectionsList]

        sections3 :: [Section]
        sections3 = sectionsList \\ (sections1 ++ sections2)

    currentRoute <- handlerToWidget getCurrentRoute
    [whamlet|
    <nav class="hidden-print">
      <ul class="nav nav-tabs nav-justified">
        <li role="presentation" :Just IndexR == currentRoute:.active>
          <a href="@{IndexR}">Главная
        $forall (Section st sd) <- sections1
          <li role="presentation" :Just st == routeToSectionType currentRoute:.active>
            <a href="@{sectionTypeToRoute st}">#{menuTitle sd}
        $forall (Section st sd) <- sections2
          <li role="presentation" .hidden-xs .hidden-sm :Just st == routeToSectionType currentRoute:.active>
            <a href="@{sectionTypeToRoute st}">#{menuTitle sd}
        <li class="dropdown">
          <a class="dropdown-toggle" data-toggle="dropdown" href="#">Ещё #
            <span class="caret">
          <ul class="dropdown-menu menu-white-link">
            $forall (Section st sd) <- sections2
              <li .hidden-md .hidden-lg :Just st == routeToSectionType currentRoute:.active>
                <a href="@{sectionTypeToRoute st}">#{menuTitle sd}
            $forall (Section st sd) <- sections3
              <li :Just st == routeToSectionType currentRoute:.active>
                <a href="@{sectionTypeToRoute st}">#{menuTitle sd}
    |]
