{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Commonmark.Lucid
  ( Html5(..)
  , RangedHtml5(..)
  )

where

import Lucid
import qualified Data.Text as T
import Data.Semigroup       (Semigroup, (<>))
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as TR
import Text.HTML.TagSoup
import Control.Monad (unless, zipWithM_)
import Data.Char (isSpace)
import Data.Coerce (coerce)
import Network.URI (isAllowedInURI, escapeURIString)
import Commonmark.Types
import Commonmark.Extensions.Math
import Commonmark.Extensions.PipeTable
import Commonmark.Extensions.Strikethrough
import Commonmark.Extensions.Footnote

newtype Html5 = Html5 {unHtml5 :: Html ()}
  deriving (Show, Semigroup, Monoid)

instance IsInline Html5 where
  lineBreak = Html5 $ br_ [] <> "\n"
  softBreak = Html5 $ "\n"
  str t = Html5 $ toHtml t
  entity t
    | illegalCodePoint t = Html5 $ toHtml ("\xFFFD" :: Text)
    | otherwise = Html5 $ toHtmlRaw t
  escapedChar c = Html5 $ toHtml (T.singleton c)
  emph ils = Html5 $ em_ $ unHtml5 ils
  strong ils = Html5 $ strong_ $ unHtml5 ils
  link target title ils = Html5 $
    a_ (href_ (escapeURI target) : [title_ title | not (T.null title)])
    $ unHtml5 ils
  image target title ils = Html5 $
    img_ ([src_ (escapeURI target),
           alt_ (renderAlt $ unHtml5 ils)] ++
          [title_ title | not (T.null title)])
  code t = Html5 $ code_ (toHtml t)
  rawInline f t
    | f == Format "html" = Html5 $ toHtmlRaw t
    | otherwise          = Html5 $ mempty

escapeURI :: Text -> Text
escapeURI = T.pack . escapeURIString
  (\c -> isAllowedInURI c && c /= '[' && c /= ']') . T.unpack

renderAlt :: Html () -> Text
renderAlt = mconcat . map textOrAlt . parseTags . TL.toStrict . renderText
  where textOrAlt (TagText t)           = t
        textOrAlt tag@(TagOpen "img" _) = fromAttrib "alt" tag
        textOrAlt _                     = mempty

illegalCodePoint :: Text -> Bool
illegalCodePoint t =
  "&#" `T.isPrefixOf` t &&
  let t' = T.drop 2 $ T.filter (/=';') t
      badvalue (n, r) = not (T.null r) ||
                        n < 1 ||
                        n > (0x10FFFF :: Integer)
  in
  case T.uncons t' of
       Nothing -> True
       Just (x, rest)
         | x == 'x' || x == 'X'
           -> either (const True) badvalue (TR.hexadecimal rest)
         | otherwise
           -> either (const True) badvalue (TR.decimal t')

instance IsBlock Html5 Html5 where
  paragraph ils = Html5 $ p_ (unHtml5 ils) <> nl
  plain ils = Html5 $ unHtml5 ils <> nl
  thematicBreak = Html5 $ hr_ [] <> nl
  blockQuote bs = Html5 $ blockquote_ (nl <> unHtml5 bs) <> nl
  codeBlock info t = Html5 $ pre_ (with code_ attr (toHtml t)) <> nl
    where attr = [class_ ("language-" <> lang) | not (T.null info)]
          lang = T.takeWhile (not . isSpace) info
  header level ils = Html5 $ h (unHtml5 ils) <> nl
    where h = case level of
                   1 -> h1_
                   2 -> h2_
                   3 -> h3_
                   4 -> h4_
                   5 -> h5_
                   6 -> h6_
                   _ -> p_
  rawBlock f t
    | f == Format "html" = Html5 $ toHtmlRaw t
    | otherwise          = Html5 $ mempty
  referenceLinkDefinition _ _ = Html5 $ mempty
  list (BulletList _) lSpacing items = Html5 $ ul_
    (nl <> mconcat (map (li . unHtml5) items)) <> nl
   where li x = if lSpacing == TightList
                   then li_ x <> nl
                   else li_ (nl <> x) <> nl
  list (OrderedList startnum _) lSpacing items = Html5 $ with ol_ attr
    (nl <> mconcat (map (li . unHtml5) items)) <> nl
   where li x = if lSpacing == TightList
                   then li_ x <> nl
                    else li_ (nl <> x) <> nl
         attr = [start_ (T.pack (show startnum)) | startnum /= 1]

nl :: Html ()
nl = toHtmlRaw ("\n" :: Text)

newtype RangedHtml5 = RangedHtml5 {unRangedHtml5 :: Html ()}
  deriving (Show, Semigroup, Monoid)

instance IsInline RangedHtml5 where
  str t = RangedHtml5 $ span_ $ toHtml t
  escapedChar c = RangedHtml5 $ span_ $ toHtml (T.singleton c)
  entity t
    | illegalCodePoint t = RangedHtml5 $ span_ $ toHtml ("\xFFFD" :: Text)
    | otherwise = RangedHtml5 $ span_ $ toHtmlRaw t
  rawInline f t
    | f == Format "html" = RangedHtml5 $ span_ $ toHtmlRaw t
    | otherwise          = mempty
  lineBreak = coerce (lineBreak :: Html5)
  softBreak = coerce (softBreak :: Html5)
  emph ils = coerce (emph (coerce ils) :: Html5)
  strong ils = coerce (strong (coerce ils) :: Html5)
  link target title ils = coerce (link target title (coerce ils) :: Html5)
  image target title ils = coerce (image target title (coerce ils) :: Html5)
  code t = coerce (code t :: Html5)

instance IsBlock RangedHtml5 RangedHtml5 where
  paragraph ils = coerce (paragraph (coerce ils) :: Html5)
  plain ils = coerce (plain (coerce ils) :: Html5)
  thematicBreak = coerce (thematicBreak :: Html5)
  blockQuote bs = coerce (blockQuote (coerce bs) :: Html5)
  codeBlock info t = coerce (codeBlock info t :: Html5)
  header level ils = coerce (header level (coerce ils) :: Html5)
  rawBlock f t = coerce (rawBlock f t :: Html5)
  referenceLinkDefinition x y = coerce (referenceLinkDefinition x y :: Html5)
  list lt lSpacing items = coerce (list lt lSpacing (coerce items) :: Html5)

instance Rangeable RangedHtml5 where
  ranged r (RangedHtml5 x) =
    RangedHtml5 $ with x [data_ "sourcepos" (T.pack (show r))]

instance Rangeable Html5 where
  ranged _ x = x

instance HasMath Html5 where
  inlineMath t = Html5 $
    span_ [class_ ("math inline")] ("\\(" <> toHtml t <> "\\)")
  displayMath t = Html5 $
    span_ [class_ ("math display")] ("\\[" <> toHtml t <> "\\]")

instance HasMath RangedHtml5 where
  inlineMath t = coerce (inlineMath t :: Html5)
  displayMath t = coerce (displayMath t :: Html5)

instance HasPipeTable Html5 Html5 where
  pipeTable aligns headerCells rows = Html5 $ do
    let alignToAttr LeftAlignedCol    = [style_ "text-align: left;"]
        alignToAttr CenterAlignedCol  = [style_ "text-align: center;"]
        alignToAttr RightAlignedCol   = [style_ "text-align: right;"]
        alignToAttr DefaultAlignedCol = []
    let toCell constructor align cell = do
          with constructor (alignToAttr align) (unHtml5 cell)
          "\n"
    table_ $ do
      "\n"
      thead_ $ do
        "\n"
        tr_ $ do
          "\n"
          zipWithM_ (toCell th_) aligns headerCells
        "\n"
      "\n"
      unless (null rows) $ do
        tbody_ $ do
          "\n"
          mapM_ ((>> "\n") . tr_ . ("\n" >>) .
                   zipWithM_ (toCell td_) aligns) rows
        "\n"
    "\n"

instance HasPipeTable RangedHtml5 RangedHtml5 where
  pipeTable aligns headerCells rows = coerce
    (pipeTable aligns (map coerce headerCells :: [Html5])
      (map (map coerce) rows :: [[Html5]]) :: Html5)

instance HasStrikethrough Html5 where
  strikethrough ils = Html5 $ del_ (unHtml5 ils)

instance HasStrikethrough RangedHtml5 where
  strikethrough ils = coerce (strikethrough (coerce ils) :: Html5)

instance HasFootnote Html5 Html5 where
  footnote num lab' x = Html5 $ do
   with div_ [class_ "footnote", id_ ("fn-" <> lab')] $ do
     "\n"
     with div_ [class_ "footnote-number"] $ do
       "\n"
       a_ [href_ ("#fnref-" <> lab')] $ toHtml (show num)
     with div_ [class_ "footnote-contents"] $ do
       "\n"
       coerce x
     "\n"
   "\n"
  footnoteList items = Html5 $ do
    with section_ [class_ "footnotes"] $ "\n" <> coerce (mconcat items)
    "\n"
  footnoteRef x lab _ = Html5 $
    with sup_ [class_ "footnote-ref"] $
      a_ [href_ ("#fn-" <> lab), id_ ("fnref-" <> lab)] $ toHtml x

instance HasFootnote RangedHtml5 RangedHtml5 where
  footnote num lab' x = coerce (footnote num lab' (coerce x) :: Html5)
  footnoteList items = coerce (footnoteList (coerce items) :: Html5)
  footnoteRef x lab y = coerce (footnoteRef (coerce x) lab (coerce y) :: Html5)
