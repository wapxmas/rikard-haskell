{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Network.Wai.Handler.CGI
  import RikardCorp.Init

  main :: IO ()
  main = mkWaiApplication >>= run
