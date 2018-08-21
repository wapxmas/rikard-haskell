{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module RikardCorp.Handlers.Additions where

  import Yesod
  import RikardCorp.Foundation()
  import RikardCorp.Types

  getAdditionsR :: Handler Html
  getAdditionsR = defaultLayout $ do
    setTitle "Дополнительные сервисы"
    [whamlet|В разработке.|]
