{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module RikardCorp.Handlers.Robots where

  import Yesod
  import RikardCorp.Foundation()
  import RikardCorp.Types
  import Data.FileEmbed (embedFile)

  getRobotsR :: Handler TypedContent
  getRobotsR =
    return $ TypedContent typePlain
           $ toContent $(embedFile "embed/robots.txt")
