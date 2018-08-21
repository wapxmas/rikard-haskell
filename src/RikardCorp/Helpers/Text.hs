{-# LANGUAGE OverloadedStrings #-}

module RikardCorp.Helpers.Text where

  import qualified Data.Text as T
  import qualified Data.Char as C
  import qualified Data.List as L
  import Data.Function
  import Data.Maybe

  stripAroundText :: T.Text -> T.Text -> T.Text
  stripAroundText pt txt1 =
    let
      step1 = T.stripPrefix pt txt1
      txt2 = fromMaybe txt1 step1
      step2 = T.stripSuffix pt txt2
      txt3 = fromMaybe txt2 step2
    in
      txt3

  capitalString :: T.Text -> T.Text
  capitalString txt = let txt' = T.toLower txt in
    case T.uncons txt' of
      Just (c, t) -> C.toUpper c `T.cons` t
      _ -> txt'

  cleanCRLF :: T.Text -> T.Text
  cleanCRLF = T.filter (`L.notElem` ("\r\n" :: String))

  hideCRLF :: T.Text -> T.Text
  hideCRLF = T.map (\ch -> if ch `L.notElem` ("\r\n" :: String) then ch else ' ')

  cleanString :: T.Text -> T.Text
  cleanString = T.dropAround (\c -> C.isSpace c || C.isSeparator c)

  isWhiteSpace :: C.Char -> Bool
  isWhiteSpace c = C.isSpace c || C.isSeparator c

  cleanText :: T.Text -> T.Text
  cleanText = T.unlines . cleanText' . T.lines

  cleanText' :: [T.Text] -> [T.Text]
  cleanText' = L.filter (not . T.null) . L.map cleanString

  stripScreening :: T.Text -> T.Text
  stripScreening = T.replace "\\\"" ""

  cleanExcessSpaces :: T.Text -> T.Text
  cleanExcessSpaces = T.concat . L.map (T.take 1) . T.groupBy ((&&) `on` isWhiteSpace)
