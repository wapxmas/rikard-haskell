{-# LANGUAGE OverloadedStrings #-}

module RikardCorp.Helpers.Text.Textile where

  import           Text.Pandoc.Options
  import qualified Text.Pandoc.Readers.Textile as PAN
  import qualified Text.Pandoc.Writers.HTML    as PAN
  import qualified Data.Text as T
  import qualified Data.Char as C
  import qualified Data.List as DL
  import           Yesod
  import qualified RikardCorp.Helpers.Html     as HH
  import qualified RikardCorp.Helpers.NLP.Text as HNT
  import qualified RikardCorp.Helpers.Text     as HT
  import qualified Data.HashSet                as H

  type SectionFixings = [T.Text -> Maybe T.Text]
  type TextAdditions = T.Text
  type TextDeletions = [T.Text]
  type ExcessedText = H.HashSet T.Text

  type LinkName = T.Text
  type LinkUrl = T.Text

  writerOptions :: WriterOptions
  writerOptions = def {writerHtml5 = True, writerEmailObfuscation = ReferenceObfuscation}

  parsePreformattedText :: T.Text -> Html
  parsePreformattedText newstext =
    case PAN.readTextile def (T.unpack newstext) of
      Right pandoc -> PAN.writeHtml writerOptions pandoc
      _ -> toHtml newstext

  fixSectionUppercased :: T.Text -> Maybe T.Text
  fixSectionUppercased txt
    | tr <- T.filter C.isLetter txt,
      not . T.null $ tr,
      T.all C.isUpper tr = mkStrong txt
    | otherwise = Nothing

  mkStrong :: T.Text -> Maybe T.Text
  mkStrong txt = Just $ '*' `T.cons` txt `T.snoc` '*'

  fixSectionColon :: T.Text -> Maybe T.Text
  fixSectionColon txt = fixSectionColon' (T.last txt)
    where
      fixSectionColon' :: Char -> Maybe T.Text
      fixSectionColon' ':' = mkStrong txt
      fixSectionColon' _ = Nothing

  fixSectionStandalone :: T.Text -> Maybe T.Text
  fixSectionStandalone txt
    | T.length txt > 1,
      ch1 <- T.head txt,
      ch2 <- T.last txt,
      C.isLetter ch1 && C.isUpper ch1 && C.isLetter ch2 = mkStrong txt
    | otherwise = Nothing

  skipPrefixes :: T.Text -> Maybe T.Text
  skipPrefixes txt
    | not . T.null $ txt,
      T.head txt `elem` skipPrefixes' = Just txt
    | otherwise = Nothing
    where
      skipPrefixes' :: String
      skipPrefixes' = "*#"

  fixings :: SectionFixings
  fixings = [skipPrefixes, fixSectionColon, fixSectionUppercased, fixSectionStandalone]

  fixSections :: [T.Text] -> [T.Text]
  fixSections (t:ts)
    | not . T.null $ t = fixSection t : fixSections ts
    | otherwise = t : fixSections ts
    where
      fixSection :: T.Text -> T.Text
      fixSection txt = fixSection' fixings txt Nothing
        where
          fixSection' :: SectionFixings -> T.Text -> Maybe T.Text -> T.Text
          fixSection' _ _ (Just tr) = tr
          fixSection' [] tx _ = tx
          fixSection' (f:fs) tx _ = fixSection' fs tx (f tx)
  fixSections ts = ts

  fixListStrings :: [T.Text] -> [T.Text]
  fixListStrings (t1:tn:t2:ts) =
    fixString (T.uncons t1) (T.uncons t2) (T.null tn)
    where
      fixString :: Maybe (Char, T.Text) -> Maybe (Char, T.Text) -> Bool -> [T.Text]
      fixString (Just ('*', _)) (Just ('*', _)) True = t1 : fixListStrings (t2:ts)
      fixString (Just ('#', _)) (Just ('#', _)) True = t1 : fixListStrings (t2:ts)
      fixString _ _ _ = t1 : fixListStrings (tn:t2:ts)
  fixListStrings ts = ts

  incorrectListPrefixes :: String
  incorrectListPrefixes = "-·•ü"

  listPrefix :: Char
  listPrefix = '*'

  fixListPrefix :: [T.Text] -> [T.Text]
  fixListPrefix =
    map (\x -> case T.uncons x of
      Just (ch, t) ->
        if ch `elem` incorrectListPrefixes
          then listPrefix `T.cons` t
          else x
      _ -> x)

  fixListPrefixNumberred :: [T.Text] -> [T.Text]
  fixListPrefixNumberred (t:ts) = fixListPrefixNumberred' t : fixListPrefixNumberred ts
    where
      fixListPrefixNumberred' txt
        | (tnum, trest) <- T.break (== ')') txt,
          not . T.null $ tnum,
          not . T.null $ trest,
          T.all C.isDigit tnum = "# " `T.append` T.tail trest
        | otherwise = txt
  fixListPrefixNumberred [] = []

  originLink :: LinkName -> LinkUrl -> T.Text
  originLink ln lu = T.concat ["p.\"", ln, "\":", lu]

  removeTextFragments :: TextDeletions -> [T.Text] -> [T.Text]
  removeTextFragments _ [] = []
  removeTextFragments delt (t:ts)
    | t `elem` delt = next
    | otherwise = t : next
    where
      next :: [T.Text]
      next = removeTextFragments delt ts

  preformatTextDef :: T.Text -> T.Text
  preformatTextDef = preformatText H.empty T.empty mempty

  preformatText :: ExcessedText -> TextAdditions -> TextDeletions -> T.Text -> T.Text
  preformatText excct addt delt = T.unlines . fixSections . fixListStrings . fixListPrefixNumberred . fixListPrefix . DL.intersperse T.empty .
    flip (++) (HT.cleanText' . T.lines $ addt) . removeTextFragments delt . HNT.stripExceedText excct . HT.cleanText' . T.lines . HH.stripNBSP
