{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module RikardCorp.Handlers.Letter where

  import Yesod
  import RikardCorp.Foundation()
  import RikardCorp.Types
  import qualified Data.Text as T
  import qualified RikardCorp.Widgets as W
  import qualified RikardCorp.Helpers.Email as HE
  import qualified RikardCorp.Helpers.Html as HH
  import Data.Maybe

  data Letter = Letter { letterPhone :: Maybe T.Text, letterEmail :: Maybe T.Text, letterBody :: Maybe T.Text }

  getLetterR :: Handler Html
  getLetterR = defaultLayout $ letterWidget Nothing Nothing

  postLetterR :: Handler Html
  postLetterR = do
    letter <- runInputPost $ Letter
                <$> iopt textField "phone"
                <*> iopt textField "email"
                <*> iopt textField "mailtxt"
    case checkLetterForm letter of
      Just ew -> defaultLayout $ letterWidget (Just ew) (Just letter)
      _ -> do
        liftIO $ HE.sendCorpMail "yashin.sergey@gmail.com" "Письмо" (htmlMessage letter)
        defaultLayout $ W.successJumbo "Спасибо!" "Ваше письмо успешно отправлено."

  htmlMessage :: Letter -> Html
  htmlMessage letter =
    let
      body = HH.nl2br . fromJust . letterBody $ letter
    in
    [shamlet|
    <div style="font-size:14px;">
      <div>E-Mail: #{fromJust (letterEmail letter)}
      <div style='margin-top:10px;'>Телефон: #{fromJust (letterPhone letter)}
      <div style="font-size:14px;margin-top:20px;">#{preEscapedToMarkup body}
    |]

  checkLetterForm :: Letter -> Maybe Widget
  checkLetterForm (Letter Nothing _ _) = Just . W.errorMessageWidget $ "Укажите телефон"
  checkLetterForm (Letter _ Nothing _) = Just . W.errorMessageWidget $ "Укажите e-mail адрес"
  checkLetterForm (Letter _ _ Nothing) = Just . W.errorMessageWidget $ "Введите текст письма"
  checkLetterForm _ = Nothing

  letterWidget :: Maybe Widget -> Maybe Letter -> Widget
  letterWidget messageWidget letter = do
    setTitle "Написать нам"
    [whamlet|
    <section>
      <h1 class="text-center">Написать нам
      $maybe m <- messageWidget
        ^{m}
      <div class="row">
        <div class="col-lg-8 col-lg-offset-2 col-md-8 col-md-offset-2 col-sm-8 col-sm-offset-2 col-xs-10 col-xs-offset-1">
          <form class="form-horizontal" action="@{LetterR}" method="post">
            <div class="form-group">
              <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
                <label for="inputPhone">Ваш телефон:
                <input type="tel" class="form-control" id="inputPhone" placeholder="телефон" name="phone" value="#{W.maybeTextField letter letterPhone}">
            <div class="form-group">
              <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
                <label for="inputEmail">Ваш Email:
                <input type="email" class="form-control" id="inputEmail" placeholder="email" name="email" value="#{W.maybeTextField letter letterEmail}">
            <div class="form-group">
              <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
                <label for="inputText">Текст письма:
                <textarea class="form-control" id="inputText" placeholder="текст" rows="6" name="mailtxt" value="#{W.maybeTextField letter letterBody}">
            <div class="form-group">
              <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
                <button type="submit" class="btn btn-default">Отправить письмо
    |]
