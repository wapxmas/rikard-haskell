{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module RikardCorp.Helpers.Internal.Articles where

  import qualified Data.Text                       as T
  import qualified Data.Text.Lazy                  as TL
  import qualified RikardCorp.Helpers.Text.Textile as HTT
  import           Text.Blaze.Html.Renderer.Text   (renderHtml)

  type ArticleBody = T.Text

  genHtmlArticle :: ArticleBody -> T.Text
  genHtmlArticle = TL.toStrict . renderHtml . HTT.parsePreformattedText . HTT.preformatTextDef
