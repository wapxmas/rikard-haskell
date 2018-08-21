{-# LANGUAGE OverloadedStrings #-}

module RikardCorp.Helpers.NLP.Text where

import           Control.Arrow
import qualified Data.Char               as C
import           Data.Function
import qualified Data.HashSet            as H
import qualified Data.List               as DL
import qualified Data.Ord                as DO
import qualified Data.Text               as T
import qualified NLP.Mystem              as MSTEM
import qualified NLP.PStemmer.Ru         as STEM
import qualified RikardCorp.Helpers.IO   as HIO
import qualified RikardCorp.Helpers.Text as HT
import           Text.Printf

type StopWordsList = H.HashSet T.Text
type ExcessedTextList = H.HashSet T.Text
type WOriginal = (T.Text, WPosInText)
type WPosInText = Int
type WBase = T.Text
type WordScore = ((WOriginal, WBase), Int)
type WordsStat = [WordScore]
type KeyWord = (MSTEM.MSRes, WordScore)
type KeyWords = [KeyWord]

quoted :: T.Text -> T.Text
quoted t = '"' `T.cons` t `T.snoc` '"'

sortWords :: T.Text -> T.Text
sortWords = T.unwords . DL.sort . T.words

mkKeywordsLitStrings :: ExcessedTextList -> StopWordsList -> T.Text -> [T.Text]
mkKeywordsLitStrings exceedTexts stopWords = removeEmptyString . removeStopWords stopWords .
  removeLatinWords . removeReductions . removeSmallWords . keepOnlyLetters .
  stripExceedText exceedTexts . corrReductions . splitLitStrings . replaceQuotes

mkStdKeywords :: StopWordsList -> KeyWords -> [T.Text]
mkStdKeywords stp ks =
  let
    kPrepared :: KeyWords
    kPrepared = DL.filter (\(res, _) -> not (MSTEM.isPersn res || MSTEM.isPatrn res)) ks

    kReversed :: KeyWords
    kReversed = DL.reverse kPrepared
  in
    uniqueWords (
      takeWithStopwords stp 6 (takeMainKeywords kReversed) ++
      takeWithStopwords stp 6 (takeFamnames kReversed) ++
      takeWithStopwords stp 3 (takeLastKeywords kPrepared) ++
      takeWithStopwords stp 3 (takeGEOKeywords ks))

getOriginalWords :: WordsStat -> [T.Text]
getOriginalWords = map (\(((wo, _), _), _) -> wo)

uniqueWords :: [T.Text] -> [T.Text]
uniqueWords = uniqueText' H.empty
  where
    uniqueText' :: H.HashSet T.Text -> [T.Text] -> [T.Text]
    uniqueText' _ [] = []
    uniqueText' hs (t:ts) =
      if H.member t hs
        then uniqueText' hs ts
        else t : uniqueText' (H.insert t hs) ts

takeWithStopwords :: StopWordsList -> Int -> [T.Text] -> [T.Text]
takeWithStopwords stp size = DL.take size . DL.filter (not . flip H.member stp)

takeFamnames :: KeyWords -> [T.Text]
takeFamnames = map (\(res, _) -> MSTEM.getRWordRes res) . DL.filter (\(res, _) -> MSTEM.isFamname res)

takeLastKeywords :: KeyWords -> [T.Text]
takeLastKeywords = map (\(res, _) -> MSTEM.getRWordRes res) . DL.filter (\(res, _) -> MSTEM.isNoun res)

takeGEOKeywords :: KeyWords -> [T.Text]
takeGEOKeywords = map (\(res, _) -> MSTEM.getRWordRes res) . DL.filter (\(res, _) -> MSTEM.isGEO res)

takeMainKeywords :: KeyWords -> [T.Text]
takeMainKeywords = map (\(res, _) -> MSTEM.getRWordRes res) . DL.filter (\(res, _) -> MSTEM.isNoun res)

removeEmptyString :: [T.Text] -> [T.Text]
removeEmptyString = DL.takeWhile (\w -> T.length w > 0)

removeStopWords :: StopWordsList -> [T.Text] -> [T.Text]
removeStopWords stopWords = map (T.unwords . filter (\w -> not $ H.member w stopWords) . T.words)

getStopWords :: FilePath -> IO StopWordsList
getStopWords fp = H.fromList . DL.takeWhile (\w -> T.length w > 0) . T.lines <$> HIO.readFileUtf8 fp

showWordsStat :: WordsStat -> IO ()
showWordsStat = mapM_ (\(((wo, _), wb), i) -> printf "%s : %s %d\n" (T.unpack wo) (T.unpack wb) i)

showKeywordsStat :: KeyWords -> IO ()
showKeywordsStat = mapM_ (\(MSTEM.MSRes _ (MSTEM.RWord rw pos grams _ : _), (((wo, _), wb), i)) ->
  printf "%s : %s : %s, %s -- %s %d\n" (T.unpack wo) (T.unpack wb) (T.unpack rw) (show pos) (show grams) i)

wordsStat :: [T.Text] -> WordsStat
wordsStat = DL.sortBy (DO.comparing snd) . map (head &&& length) . DL.groupBy ((==) `on` snd) .
  DL.sortBy (DO.comparing snd) . map (id &&& STEM.runPorter . fst) . wordsFromList

wordsFromList :: [T.Text] -> [WOriginal]
wordsFromList = flip DL.zip [1..] . concatMap T.words

removeLatinWords :: [T.Text] -> [T.Text]
removeLatinWords = map removeLatinWords'

removeLatinWords' :: T.Text -> T.Text
removeLatinWords' = T.unwords . filter (not . T.all C.isLatin1) . T.words

removeReductions :: [T.Text] -> [T.Text]
removeReductions = map (T.unwords . filter (\w -> not $ H.member w reductionsList) . T.words)

removeSmallWords :: [T.Text] -> [T.Text]
removeSmallWords = map (T.unwords . filter (\w -> T.length w > 2) . T.words)

keepOnlyLettersAndNums :: [T.Text] -> [T.Text]
keepOnlyLettersAndNums = map (T.toLower . HT.cleanString . HT.cleanExcessSpaces . keepOnlyLettersAndNums')

keepOnlyLettersAndNums' :: T.Text -> T.Text
keepOnlyLettersAndNums' = T.map (\ch -> if C.isLetter ch || C.isDigit ch then ch else ' ')

keepOnlyLetters :: [T.Text] -> [T.Text]
keepOnlyLetters = map (T.toLower . HT.cleanString . HT.cleanExcessSpaces . keepOnlyLetters')

keepOnlyLetters' :: T.Text -> T.Text
keepOnlyLetters' = T.map (\ch -> if C.isLetter ch then ch else ' ')

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

replaceQuotes :: T.Text -> T.Text
replaceQuotes = T.map replaceCh
  where
    replaceCh :: C.Char -> C.Char
    replaceCh '«' = '"'
    replaceCh '»' = '"'
    replaceCh '„' = '"'
    replaceCh '“' = '"'
    replaceCh '”' = '"'
    replaceCh '‘' = '"'
    replaceCh '’' = '"'
    replaceCh ch = ch

stripExceedText :: H.HashSet T.Text -> [T.Text] -> [T.Text]
stripExceedText exceedTexts = DL.takeWhile (\s ->
  let (hs, ls) = (T.toLower . head &&& T.toLower . last) $ T.lines s in
    not $ ((||) `on` flip H.member exceedTexts) hs ls)

corrReductions :: [T.Text] -> [T.Text]
corrReductions = corrBrackets . corrQuotes1 . corrHardReduct2 . corrHardReduct1 . corrSimpleReduct2 . corrSimpleReduct1

corrBrackets :: [T.Text] -> [T.Text]
corrBrackets (s1:s2:xs)
  | T.null s1 = toNextString
  | otherwise =
    let
      bracketsC :: Int
      bracketsC = T.foldl (\n ch -> if isBracket ch then n + 1 else n) 0 s1
    in
      if bracketsC > 0 && odd bracketsC
        then corrBrackets $ s1 `T.append` (' ' `T.cons` s2) : xs
        else toNextString
  where
    toNextString :: [T.Text]
    toNextString = s1 : corrBrackets (s2 : xs)

    isBracket :: C.Char -> Bool
    isBracket ch = ch == '(' || ch == ')'
corrBrackets xs = xs

corrQuotes1 :: [T.Text] -> [T.Text]
corrQuotes1 (s1:s2:xs)
  | T.null s1 = toNextString
  | otherwise =
    if isUnclosedQuote s1
      then corrQuotes1 $ s1 `T.append` (' ' `T.cons` s2) : xs
      else toNextString
  where
    toNextString :: [T.Text]
    toNextString = s1 : corrQuotes1 (s2 : xs)
corrQuotes1 xs = xs

isUnclosedQuote :: T.Text -> Bool
isUnclosedQuote txt
  | T.null txt = False
  | T.last txt == '"' = False
  | otherwise = isUnclosedQuote' . reverse $ T.inits txt
  where
    isUnclosedQuote' :: [T.Text] -> Bool
    isUnclosedQuote' (s1:s2:xs)
      | T.length s1 > 1 && T.last s2 == '"' = C.isLetter (T.last s1)
      | otherwise = isUnclosedQuote' (s2 : xs)
    isUnclosedQuote' _ = False

corrHardReduct2 :: [T.Text] -> [T.Text]
corrHardReduct2 (s1:s2:xs)
  | T.null s1 = toNextString
  | T.null s2 = toNextString
  | otherwise =
    let
      s1Ch :: C.Char
      s1Ch = T.last s1

      s2Ch :: C.Char
      s2Ch = T.head s2
    in
      if ((&&) `on` C.isNumber) s1Ch s2Ch
        then corrHardReduct2 $ s1 `T.append` (' ' `T.cons` s2) : xs
        else toNextString
  where
    toNextString :: [T.Text]
    toNextString = s1 : corrHardReduct2 (s2 : xs)
corrHardReduct2 xs = xs

corrHardReduct1 :: [T.Text] -> [T.Text]
corrHardReduct1 (s1:s2:xs)
  | T.null s1 = toNextString
  | otherwise =
    let
      lastWord :: T.Text
      lastWord = T.toLower $ T.takeWhileEnd C.isLetter s1
    in
      if H.member lastWord reductionsList
        then corrHardReduct1 $ s1 `T.append` (' ' `T.cons` s2) : xs
        else toNextString
  where
    toNextString :: [T.Text]
    toNextString = s1 : corrHardReduct1 (s2 : xs)
corrHardReduct1 xs = xs

reductionsList :: H.HashSet T.Text
reductionsList = H.unions [reductionsList1, reductionsList2]

reductionsList2 :: H.HashSet T.Text
reductionsList2 = H.fromList ["в", "гг", "руб", "каб", "дж", "тел", "прим", "см", "тыс", "доб", "млн", "чел"]

reductionsList1 :: H.HashSet T.Text
reductionsList1 = H.fromList ["г","дер","бул","бр","буль","бульв","наб","набережн","набереж","пл","пер","просп","пр","ул","ш","туп","д","к","корп","кор","стр","вл","влад","соор","обл","пос","ал","алл","м","мкр"]

corrSimpleReduct1 :: [T.Text] -> [T.Text]
corrSimpleReduct1 (s1:s2:xs)
  | T.null s1 = toNextString
  | otherwise =
    let
      lastWord :: T.Text
      lastWord = last $ T.words s1
    in
      if T.length lastWord == 1 && C.isUpper (T.head lastWord)
        then corrSimpleReduct1 $ s1 `T.append` (' ' `T.cons` s2) : xs
        else toNextString
  where
    toNextString :: [T.Text]
    toNextString = s1 : corrSimpleReduct1 (s2 : xs)
corrSimpleReduct1 xs = xs

corrSimpleReduct2 :: [T.Text] -> [T.Text]
corrSimpleReduct2 (s1:s2:xs) =
  case T.uncons s2 of
    Just (ch, _)  | C.isLower ch -> corrSimpleReduct2 $ s1 `T.append` (' ' `T.cons` s2) : xs
                  | otherwise -> toNextString
    _ -> toNextString
  where
    toNextString :: [T.Text]
    toNextString = s1 : corrSimpleReduct2 (s2 : xs)
corrSimpleReduct2 xs = xs

splitLitStrings :: T.Text -> [T.Text]
splitLitStrings txt =
  let
    (str', rest') = getLitString txt
    (str, rest) = (HT.cleanString str', rest')
  in
    if T.null rest
      then
        if T.null str
          then []
          else [str]
      else
        if T.null str
          then splitLitStrings rest
          else str : splitLitStrings rest

getLitString :: T.Text -> (T.Text, T.Text)
getLitString txt =
  case T.uncons txt of
    Just (ch, xs) | ch `elem` endLitStringChars -> (T.empty, T.dropWhile (`elem` endLitStringChars) xs)
                  | otherwise ->
                                let
                                  r :: (T.Text, T.Text)
                                  r = getLitString xs
                                in
                                  (ch `T.cons` fst r, snd r)
    _ -> (T.empty, txt)

endLitStringChars :: String
endLitStringChars = ['.','!','?','‽']
