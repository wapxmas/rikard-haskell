{-# LANGUAGE OverloadedStrings #-}

module RikardCorp.Helpers.Html where

  import qualified Data.Text         as T
  import qualified Text.HTML.TagSoup as TS
  import qualified HTMLEntities.Decoder as HE

  nl2br :: T.Text -> T.Text
  nl2br = T.replace "\n" "<br/>" . T.filter ('\r' /=)

  stripNBSP :: T.Text -> T.Text
  stripNBSP = T.map (\c -> if c == '\160' then ' ' else c)

  stripTags :: T.Text -> T.Text
  stripTags txt = if T.null txt then txt else stripTags' txt T.empty
    where
      stripTags' :: T.Text -> T.Text -> T.Text
      stripTags' itxt otxt = if T.null itxt then otxt else
        (let
            ch :: Char
            ch = T.head itxt
           in
            case ch of
              '<' -> stripTags' (T.drop 1 $ T.dropWhile (/= '>') itxt) otxt
              _ -> stripTags' (T.tail itxt) (T.snoc otxt ch))

  pTagToNewLine :: T.Text -> T.Text
  pTagToNewLine = T.replace "</p>" "\n"

  brTagToNewLine :: T.Text -> T.Text
  brTagToNewLine = T.replace "< br/>" "\n" . T.replace "<br/ >" "\n" . T.replace "<br />" "\n" . T.replace "<br/>" "\n" . T.replace "<br>" "\n"

  firstTagTextIs :: T.Text -> [TS.Tag T.Text] -> Bool
  firstTagTextIs _ [] = False
  firstTagTextIs match (TS.TagText txt:_) = txt == match
  firstTagTextIs match (_:ts) = firstTagTextIs match ts

  decodeHtmlEntities :: T.Text -> T.Text
  decodeHtmlEntities s = case T.uncons s of
      Just ('&', xs) ->
            let
              (entity, after) = T.break (==';') xs

              entityTxt :: T.Text
              entityTxt = T.concat ["&",entity,";"]

              entityDecoded :: T.Text
              entityDecoded =
                case HE.htmlEntity entityTxt of
                  Right tr -> tr
                  _ -> entityTxt
            in
              entityDecoded `T.append` decodeHtmlEntities (T.tail after)

      Just (x, xs) -> x `T.cons` decodeHtmlEntities xs

      _ -> T.empty
