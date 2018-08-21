{-# LANGUAGE OverloadedStrings #-}

module RikardCorp.Helpers.Email where

  import           Network.HaskellNet.SMTP
  import           Network.HaskellNet.SMTP.SSL
  import           Network.Mail.Mime
  import qualified Data.Text as T
  import qualified Data.ByteString.Char8       as B8
  import qualified Data.ByteString.Lazy        as BL
  import Control.Monad
  import Control.Exception
  import Text.Hamlet
  import Text.Blaze.Html.Renderer.Text (renderHtml)

  type Recipient = String
  type Subject = T.Text
  type HtmlBody = Html

  sendCorpMail :: Recipient -> Subject -> HtmlBody -> IO ()
  sendCorpMail to subject htmlSrc = do
    let
      server       = "smtp.yandex.ru"
      username     = "info@rikard.ru"
      password     = "verbatim123456"
      authType     = LOGIN
      htmlM        = renderHtml htmlSrc

    doSMTPSSL server $ \conn -> do
      authSuccess <- authenticate authType username password conn
      when authSuccess $ do
        let newMail = Mail (Address (Just "Рикард-Недвижимость") "info@rikard.ru")
                           [Address Nothing $ T.pack to] [] []
                           [("Subject", subject)]
                           [[htmlPart htmlM]]
        renderedMail <- renderMail' newMail
        handle (\e -> print (e :: SomeException)) $ sendMail username
          [to] (B8.concat . BL.toChunks $ renderedMail) conn
