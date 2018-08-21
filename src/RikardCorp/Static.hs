{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module RikardCorp.Static where

  import Yesod.EmbeddedStatic

  mkEmbeddedStatic False "rikardStatic"
    [
      embedDir "static"
    ]
