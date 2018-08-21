{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module RikardCorp.Handlers.Contacts where

  import Yesod
  import RikardCorp.Foundation()
  import RikardCorp.Types

  getContactsR :: Handler Html
  getContactsR = defaultLayout $ do
    setTitle "Контакты"
    [whamlet|
    <section>
      <h1 class="text-center">Контакты
      <div class="row">
        <div class="col-lg-8 col-lg-offset-2 col-md-8 col-md-offset-2 col-sm-8 col-sm-offset-2 col-xs-10 col-xs-offset-1">
          <address>
            <div>Телефон: #
              <a href="tel:+74956652098" style="text-decoration: none;">
                <span style="color: #000000;">+7 (495) #
                  <strong style="font-size: 16px; color: #000000;">665-20-98
            <div style="margin-top: 15px;">Адрес: 109012, Россия, Москва, улица Ильинка, 4
            <div style="margin-top: 15px;">Электронная почта: #
              <a href="mailto:info@rikard.ru">info@rikard.ru
          <div style="margin-top: 15px;">
            <span>Мы в социальных сетях и лентах:
            <ul style="padding-left: 20px; padding-top: 5px;" class="list-unstyled">
              <li style="margin-top: 5px;">&mdash; #
                <a href="https://www.facebook.com/rikard.ru" target="_blank">Facebook
              <li style="margin-top: 5px;">&mdash; #
                <a href="https://twitter.com/rikard_ru" target="_blank">Twitter
    |]
