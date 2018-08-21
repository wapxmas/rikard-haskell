{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RikardCorp.Foundation where

  import qualified Data.Text            as T
  import qualified Data.Text.Lazy            as TL
  import           RikardCorp.Helpers.DateTime
  import           RikardCorp.Static
  import RikardCorp.Types
  import           Text.Jasmine         (minifym)
  import Text.Hamlet
  import           Yesod
  import           Yesod.EmbeddedStatic
  import           Database.Persist.MySQL
  import Yesod.Form.I18n.Russian

  instance RenderMessage RikardCorp FormMessage where
      renderMessage _ _ = russianFormMessage

  instance YesodPersist RikardCorp where
      type YesodPersistBackend RikardCorp = SqlBackend
      runDB action = do
          mpool <- connectionsPool <$> getYesod
          flip (maybe (error "No database is specified")) mpool $ \pool ->
            runSqlPool action pool

  instance Yesod RikardCorp where
    errorHandler NotFound =
      toTypedContent <$> uniErrorHandler "Не найдено!" "Мы сожалеем, запрашиваемая информация не найдена."
    errorHandler (PermissionDenied _) =
      toTypedContent <$> uniErrorHandler "Доступ запрещен" "Мы сожалеем, доступ к запрашиваемому ресурсу запрещен."
    errorHandler _ =
      toTypedContent <$> uniErrorHandler "Внутренняя ошибка" "Мы сожалеем, возможно сервер перегружен, попробуйте зайти позднее или обратитесь по телефону."

    addStaticContent =
      embedStaticContent getStatic StaticR minifym

    defaultLayout contents = do
      currentRoute <- getCurrentRoute
      rendererP <- getUrlRenderParams
      currentYear <- liftIO getCurrentYear
      PageContent pTitle headTags bodyTags <- widgetToPageContent $ do
        addStylesheet $ StaticR bootstrap3_css_bootstrap_min_css
        addStylesheet $ StaticR custom_css
        addScript $ StaticR jquery_min_js
        addScript $ StaticR bootstrap3_js_bootstrap_min_js
        cssGlobalStyles
        contents
        yandexMetrikaCounter
        googleAnalyticsCounter
      withUrlRenderer [hamlet|
      $doctype 5
      <html lang="ru" prefix="og: http://ogp.me/ns#">
        <head>
          <meta charset="utf-8">
          <meta http-equiv="X-UA-Compatible" content="IE=edge">
          <meta name="viewport" content="width=device-width, initial-scale=1">
          <meta name="format-detection" content="telephone=no">
          <meta name="yandex-verification" content="7b5911f7ce0c0318">
          <meta name="yandex-verification" content="7c7c10ef3abdca5b">
          <meta name="google-site-verification" content="f1OasrKgM7DyZZORCCwiXvOeZlllmBmk6dHBnIBIoU0">
          <meta name="google-site-verification" content="sJ90VtoXczg9FrCAwJ29SlvwLEUe5g8uyA5ej4VezJo">
          <meta property="og:type" content="website">
          <meta property="og:site_name" content="Рикард-Недвижимость">
          <meta property="og:image" content="@{StaticR img_logo_png}">
          <meta property="og:title" content="#{pTitle}">
          <meta property="og:email" content="info@rikard.ru">
          <meta property="og:phone_number" content="+7 495 665 20 98">
          <title>#{pTitle}
          ^{headTags}
        <body>
          ^{pageHeader currentRoute}
          <main .container>
            ^{bodyTags}
          ^{pageFooter currentYear rendererP}
      |]

  uniErrorHandler :: T.Text -> T.Text -> Handler Html
  uniErrorHandler pTitle pText = defaultLayout $ do
    setTitle $ toHtml pTitle
    [whamlet|
    <section class="jumbotron">
      <h2>#{pTitle}
      <p>#{pText}
      <p>
        <a class="btn btn-primary btn-lg" href="@{IndexR}" role="button">Перейти на главную
    |]

  pageFooter :: Integer -> Render (Route RikardCorp) -> HtmlUrl (Route RikardCorp)
  pageFooter currentYear renderer =
    [hamlet|
    <div class="container hidden-print">
      <hr>
      <footer class="row footer hidden-xs">
        <div class="col-lg-1 hidden-sm hidden-md clearfix">&nbsp;
        <div class="col-lg-5 col-md-6 col-sm-6 clearfix" style="padding-bottom: 5px;">
          <p class="copyright">© «Рикард-Недвижимость», 2012&ndash;#{currentYear}<br>
            Коммерческая недвижимость<br>в аренду от города Москвы
          ^{liveinternetCounter renderer}
        <div class="col-lg-5 col-md-6 col-sm-6 clearfix">
          <p class="phone">(495) 665-20-98
            <span class="phone-time"><br>&mdash; с 9-00 до 18-00
        <div class="col-lg-1 hidden-sm hidden-md clearfix">&nbsp;
      <footer class="row visible-xs">
        <div class="col-xs-12 text-center" style="padding-bottom: 5px;">
          ^{liveinternetCounter renderer}
    |]

  pageHeader :: Maybe (Route RikardCorp) -> HtmlUrl (Route RikardCorp)
  pageHeader route =
    [hamlet|
    <header>
      <nav class="navbar navbar-black-gradient navbar-radius">
        <div class="container">
          <div class="navbar-header">
            <button type="button" class="navbar-toggle collapsed toggle-nav-btn" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
              <span class="icon-bar toggle-nav-btn-elms">
              <span class="icon-bar toggle-nav-btn-elms">
              <span class="icon-bar toggle-nav-btn-elms">
            <a class="navbar-brand hidden-lg" href="@{IndexR}">
              <img alt="логотип рикард-недвижимость" src="@{StaticR img_logomini_png}">
            <a class="navbar-brand a-color-green hilight-green" href="@{IndexR}">Рикард-Недвижимость
          <div id="navbar" class="navbar-collapse collapse">
            <ul class="nav navbar-nav main-menu-navbar">
              <li :isNewsRoute route:.menu-main-active>
                <a href="@{NewsR}">Новости торгов
              <li :Just ArticlesR == route:.menu-main-active>
                <a href="@{ArticlesR}">Статьи
              <li :Just ContactsR == route:.menu-main-active>
                <a href="@{ContactsR}">Контакты
              <li :Just FraudR == route:.menu-main-active>
                <a href="@{FraudR}">Остерегайтесь мошенников!
              <li :Just AdditionsR == route:.menu-main-active class="hidden-lg hidden-md">
                <a href="@{AdditionsR}">Сервисы для арендатора
            <p class="navbar-text navbar-right">
              <a href="https://twitter.com/rikard_ru" class="navbar-link social-link social-link2">
            <p class="navbar-text navbar-right">
              <a href="https://www.facebook.com/rikard.ru" class="navbar-link social-link social-link1">
      <div class="container">
        <div class="visible-lg content-padding-top">
          &nbsp;
        <div class="row">
          <section class="col-lg-4 col-md-4 col-sm-6 hidden-xs clearfix">
            <img class="visible-lg" style="float:left;width:93px;height:82px; margin-right:10px;" src="@{StaticR img_logo_png}" alt="логотип рикард-недвижимость">
            <h1 style="font-size: 1em; text-align: justify; line-height: inherit !important;margin-top: 0; margin-bottom: 0;">#{routeSnippet route}

          <div class="col-lg-4 col-md-4 hidden-xs hidden-sm clearfix hidden-print">
            <img style="float:left;width:74px;height:69px; margin-right:10px;" src="@{StaticR img_addfreeadv_png}" alt="cервисы для арендатора">
            <div style="padding-top:18px;">
              <a href="@{AdditionsR}">Сервисы для арендатора

          <div class="col-lg-4 visible-lg clearfix">
            <img style="float:left;width:74px;height:69px; margin-right:10px;" src="@{StaticR img_phone_ico_png}" alt="телефон рикард-недвижимость">
            <div style="margin-top:15px;margin-bottom:5px;display: inline-block;">
              <span style="font-size:27px;line-height:25px;">+7 (495) #
                <span style="font-weight: bold;">665-20-98
              <div class="text-right hidden-print" style="padding-top:5px;">
                <span class="glyphicon glyphicon-pencil glyph-color-green" aria-hidden="true">
                <a href="/w" rel="nofollow">Написать нам

          <div class="col-md-4 visible-md text-center clearfix">
            <div style="margin-top:15px;margin-bottom:5px;display: inline-block;">
              <span style="font-size:27px;line-height:25px;">
                <span class="glyphicon glyphicon-phone-alt glyph-color-green" aria-hidden="true">
                <span>+7 (495) #
                  <span style="font-weight: bold;">665-20-98
              <div class="text-right hidden-print" style="padding-top:5px;">
                <span class="glyphicon glyphicon-pencil glyph-color-green" aria-hidden="true">
                <a href="/w" rel="nofollow">Написать нам

          <div class="col-sm-6 col-xs-12 hidden-lg hidden-md text-center clearfix">
            <div style="margin-bottom:5px;display: inline-block;">
              <span style="font-size:27px;line-height:25px;">
                <span class="glyphicon glyphicon-phone-alt glyph-color-green" aria-hidden="true">
                <span>+7 (495) #
                  <span style="font-weight: bold;">665-20-98
              <div class="text-right hidden-print" style="padding-top:5px;">
                <span class="glyphicon glyphicon-pencil glyph-color-green" aria-hidden="true">
                <a href="/w" rel="nofollow">Написать нам
        <hr>
    |]

  cssGlobalStyles :: Widget
  cssGlobalStyles = toWidgetHead [lucius|
  .hx-bold {
    font-weight: bold;
  }
  .header-green {
    background-color: #b0cc1f !important;
    color: #ffffff !important;
  }
  .articles-list-short > li:not(:first-child) {
    margin-top: 15px;
  }
  .social-link1 {
      background: rgba(0, 0, 0, 0) url("@{StaticR img_social1_png}") no-repeat;
      width: 10px;
      height: 18px;
  }
  .social-link2 {
      background: rgba(0, 0, 0, 0) url("@{StaticR img_social2_png}") no-repeat;
      width: 18px;
      height: 15px;
  }
  .social-link3 {
      background: rgba(0, 0, 0, 0) url("@{StaticR img_social3_png}") no-repeat;
      width: 19px;
      height: 18px;
  }
  .social-link4 {
      background: rgba(0, 0, 0, 0) url("@{StaticR img_social4_png}") no-repeat;
      width: 16px;
      height: 16px;
  }
  h1 {
    font-size: 2em;
  }
  h2 {
      font-size: 1.5em;
  }
  h3 {
      font-size: 1.17em;
  }
  h4 {
      font-size: 1em;
  }
  h5 {
      font-size: .83em;
  }
  h6 {
      font-size: .67em;
  }
  .listHeader {
    line-height: 20px;
  }
  .listSection > li > section {
    margin-left: 15px;
  }
  .listSection > li > section > h4 {
      margin-bottom: 0;
      margin-top: 0;
  }
  .listSection > li {
    margin-top: 15px;
  }
  @font-face {
    font-family: 'Glyphicons Halflings';
    src: url('@{StaticR fonts_glyphicons_halflings_regular_eot}');
    src: url('@{StaticR fonts_glyphicons_halflings_regular_eot}?#iefix') format('embedded-opentype'),
      url('@{StaticR fonts_glyphicons_halflings_regular_woff2}') format('woff2'),
      url('@{StaticR fonts_glyphicons_halflings_regular_woff}') format('woff'),
      url('@{StaticR fonts_glyphicons_halflings_regular_ttf}') format('truetype'),
      url('@{StaticR fonts_glyphicons_halflings_regular_svg}#glyphicons_halflingsregular') format('svg');
  }
  |]

  googleAnalyticsCounter :: Widget
  googleAnalyticsCounter = toWidgetBody [julius|
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
          (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
      m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
  ga('create', 'UA-70750561-1', 'auto');
  ga('send', 'pageview');
  |]

  yandexMetrikaCounter :: Widget
  yandexMetrikaCounter = toWidgetBody [julius|
  (function (d, w, c) {
      (w[c] = w[c] || []).push(function() {
          try {
              w.yaCounter33850609 = new Ya.Metrika({
                  id:33850609,
                  clickmap:true,
                  trackLinks:true,
                  accurateTrackBounce:true,
                  webvisor:true
              });
          } catch(e) { }
      });

      var n = d.getElementsByTagName("script")[0],
          s = d.createElement("script"),
          f = function () { n.parentNode.insertBefore(s, n); };
      s.type = "text/javascript";
      s.async = true;
      s.src = "https://mc.yandex.ru/metrika/watch.js";

      if (w.opera == "[object Opera]") {
          d.addEventListener("DOMContentLoaded", f, false);
      } else { f(); }
  })(document, window, "yandex_metrika_callbacks");
  |]

  liveinternetCounter :: Render (Route RikardCorp) -> HtmlUrl (Route RikardCorp)
  liveinternetCounter = innerScript [julius|
  document.write("<a href='//www.liveinternet.ru/click' "+
  "target=_blank><img src='//counter.yadro.ru/hit?t26.1;r"+
  escape(document.referrer)+((typeof(screen)=="undefined")?"":
  ";s"+screen.width+"*"+screen.height+"*"+(screen.colorDepth?
      screen.colorDepth:screen.pixelDepth))+";u"+escape(document.URL)+
  ";h"+escape(document.title.substring(0,80))+";"+Math.random()+
  "' alt='' title='LiveInternet: показано число посетителей за"+
  " сегодня' border='0' width='88' height='15'><\/a>")
  |]

  innerScript :: JavascriptUrl (Route RikardCorp) -> Render (Route RikardCorp) -> HtmlUrl (Route RikardCorp)
  innerScript jscode renderer =
    let
      _js = TL.toStrict . renderJavascriptUrl renderer $ jscode
    in
      [hamlet|
      <script type="text/javascript">
        #{preEscapedToMarkup _js}
      |]
