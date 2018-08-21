{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module RikardCorp.Handlers.Fraud where

  import Yesod
  import RikardCorp.Foundation()
  import RikardCorp.Types

  getFraudR :: Handler Html
  getFraudR = defaultLayout $ do
    setTitle "Остерегайтесь мошенников!"
    [whamlet|
    <section .jumbotron>
      <h2>Внимание!
      <p>В настоящее время действует группа лиц, которая без всякого на то права, использует адреса этого сайта #
        ( <span style="text-decoration: underline">ppa.msk.ru</span> или <span style="text-decoration: underline">rikard.ru</span> ) для предоставления услуг.
        \ В последствии заказчики сталкиваются с людьми, не имеющими никакого отношения к компании Рикард-Недвижимость.
      <p .alert .alert-info>Обращаем Ваше внимание, что только по этим телефонам:  +7 (495) 665-20-98 и +7 (965) 112-76-53 Вы можете получить качественные заявленные на сайте услуги от нашей компании.
      <p>Если вы стали жертвой обмана с их стороны и потерпели убытки от их действий просим связаться по телефонам, указанным выше.
      <p>
        <a .btn .btn-primary .btn-lg href="@{LetterR}" role="button" rel="nofollow">Напишите нам!
    |]
