{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.Text.IO as TIO
import qualified Data.List as DL

import Data.Monoid
import Data.Maybe
import Data.Function
import Debug.Trace

main :: IO ()
main = do
  print $ getRoomsCount "строение 17 этаж 3 комнаты № 30,31,32,33; помещение в удовлетворительном состоянии; тип здания – панельное;  оснащение - охрана, 2 лифта туалеты на этаже; телекоммуникации - интернет и телефония по отдельным договорам"
  print $ getRoomsCount "нежилое помещение (этаж полуподвал, пом.1, ком 4,4а,5-16,16а, 17-22,22а,23-26)"
  print $ getRoomsCount "L: Подвал, пом. IV, комнаты №№ 1-3,5-6 помещения,60-70"
  print $ getRoomsCount "2 этаж, пом. VII, комнаты №№ 1-20,   подвал, пом I, комнаты № 17"
  print $ getRoomsCount "L: 2 этаж, помещение XVII, комнаты     32-45, 47-49"
  print $ getRoomsCount "L: 108823, Москва, поселение Рязановское, пос. Знамя Октября, д.31,Лабораторный корпус №1, тип 10-02-120, 1 этаж, комн №105(п/п16),  комн №116(п/п41),  комн №118(п/п46),  комн №119(п/п47), комн №118 а(п/п4,5)"
  print $ getRoomsCount "L: 119285, Москва, Мичуринский проспект, д. 6, строение 2, 1 этаж, помещение № 1, комнаты № № 5, 6, 7, 8, 10, 13 (часть), 17, 20,21, 22, 23"
  print $ getRoomsCount "L: г. Москва,   ул. Авиамоторная,  д.65, стр. 1 (1-ый этаж, помещение IV, комната № 22 (часть))"
  print $ getRoomsCount "D: Подвал, помещение № I, к. 14, 15, помещение № II, к. 1, 2, 6-8, помещение № IV, к. , 2-4, пристройка, к. а"

getRoomsCount :: T.Text -> Maybe Int
getRoomsCount rmsTxt' =
  let
    rmsTxt =
        cleanExcessSpaces
      . cleanString4Rooms
      . T.toLower
      $ rmsTxt'
    rmss =
      fmap (\w -> getRoomsCountAll w . (`T.breakOnAll` rmsTxt) $ w)
      getRoomsCountWords
    rmscount = sum rmss
    res
      | rmscount == 0 = Nothing
      | otherwise = Just rmscount
  in
    res
  where
    getRoomsCountAll :: T.Text -> [(T.Text, T.Text)] -> Int
    getRoomsCountAll wsep =
        sum
      . fmap getRoomsCountOne
      . mapMaybe (T.stripPrefix wsep . snd)

    getRoomsCountOne :: T.Text -> Int
    getRoomsCountOne =
        sum
      . fmap getRoomsCountCompute
      . (\(ts1, ts2) ->
          if DL.null ts2
            then ts1
            else
              ts1 <> [head . T.words . head $ ts2]
        )
      . DL.span ((==) 1 . DL.length . T.words)
      . DL.takeWhile (C.isDigit . T.head)
      . cleanText'
      . fmap removeBrackets
      . T.split (==',')

    getRoomsCountCompute :: T.Text -> Int
    getRoomsCountCompute rmsTxtComp =
      let
        ws = T.split (=='-') rmsTxtComp
      in
        if DL.length ws == 1
          then 1
          else
            getRoomsCountComputeRange ws

    getRoomsCountComputeRange :: [T.Text] -> Int
    getRoomsCountComputeRange rnge =
      let
        nms1 = (reads :: String -> [(Int, String)]) . T.unpack . head $ rnge
        nms2 = (reads :: String -> [(Int, String)]) . T.unpack . last $ rnge
      in
        case nms1 of
          [(nm1, _)] ->
            case nms2 of
              [(nm2, _)] ->
                let
                  c = nm2 - nm1
                  res
                    | c > 0 = c + 1
                    | otherwise = 0
                in
                  res
              _ -> 0
          _ -> 0

getRoomsCountWords :: [T.Text]
getRoomsCountWords =
  ["комнаты ", "комната ", "ком ", "комн.", "ком.", "комн ", "к. "]

cleanString4Rooms :: T.Text -> T.Text
cleanString4Rooms =
  T.map (\c ->
    if C.isLetter c
      || C.isDigit c
      || c == '-'
      || c == '.'
      || c == ','
      || c == '('
      || c == ')'
      then c
      else ' ')

removeBrackets :: T.Text -> T.Text
removeBrackets txt = if T.null txt then txt else removeBrackets' txt T.empty
  where
    removeBrackets' :: T.Text -> T.Text -> T.Text
    removeBrackets' itxt otxt = if T.null itxt then otxt else
      (let
          ch :: Char
          ch = T.head itxt
         in
          case ch of
            '(' -> removeBrackets' (T.drop 1 $ T.dropWhile (/= ')') itxt) otxt
            _ -> removeBrackets' (T.tail itxt) (T.snoc otxt ch))

cleanText' :: [T.Text] -> [T.Text]
cleanText' = DL.filter (not . T.null) . DL.map cleanString

cleanExcessSpaces :: T.Text -> T.Text
cleanExcessSpaces = T.concat . DL.map (T.take 1) . T.groupBy ((&&) `on` isWhiteSpace)

isWhiteSpace :: C.Char -> Bool
isWhiteSpace c = C.isSpace c || C.isSeparator c

cleanString :: T.Text -> T.Text
cleanString = T.dropAround (\c -> C.isSpace c || C.isSeparator c)

parseHouse :: T.Text -> T.Text -> Maybe T.Text
parseHouse str temp =
  if DL.any
      (`T.isSuffixOf` temp)
      ["д.", "домовлад.", "вл.", "дом", "домовладение"]
    then
      let
        str' = cleanString str
        res' = T.takeWhile C.isDigit str'
        res  =
          if T.null res'
            then T.empty
            else res'
              <> T.takeWhile
                    (\ch ->
                            C.isDigit ch
                         || C.isLetter ch
                         || ch `DL.elem` ['/','-'])
                    (T.drop (T.length res') str')
      in
        if T.null res
          then parseNext
          else
            let
              rest = cleanString . T.drop (T.length res) $ str'
              liter = getHouseLiter . T.unpack $ rest
            in
              Just . T.toUpper $ res <> liter
    else
      parseNext
  where
    parseNext :: Maybe T.Text
    parseNext =
      case T.uncons str of
        Just (ch, nextStr) ->
          parseHouse nextStr (temp `T.snoc` ch)
        _ -> Nothing

getHouseLiter :: String -> T.Text
getHouseLiter (c1:c2:_) =
  if C.isLetter c1 && C.isUpper c1 && not (C.isLetter c2)
    then T.pack [c1]
    else T.empty
getHouseLiter _ = T.empty
