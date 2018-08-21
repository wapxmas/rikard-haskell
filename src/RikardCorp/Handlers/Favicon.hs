{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module RikardCorp.Handlers.Favicon where

  import Yesod
  import RikardCorp.Foundation()
  import RikardCorp.Types
  import Data.FileEmbed (embedFile)

  getFaviconR :: Handler TypedContent
  getFaviconR = do
    cacheSeconds $ 60 * 60 * 24 * 30
    return $ TypedContent "image/x-icon"
           $ toContent $(embedFile "embed/favicon.ico")
