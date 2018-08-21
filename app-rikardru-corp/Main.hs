{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Network.Wai.Handler.Warp
  import RikardCorp.Init

  main :: IO ()
  main = do
    wai <- mkWaiApplication
    runSettings (setPort 8080 . setHost "*" $ defaultSettings) wai
