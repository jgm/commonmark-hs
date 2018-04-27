{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Commonmark.Lucid where

import Lucid
import qualified Data.Text as T
import Data.Semigroup       (Semigroup, (<>))
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as TR
import Text.HTML.TagSoup
import Control.Monad (unless, zipWithM_)
import Data.Char (isSpace)
import Network.URI (isAllowedInURI, escapeURIString)
import Commonmark.Types
import Commonmark.Extensions.Math
import Commonmark.Extensions.PipeTable
import Commonmark.Extensions.Strikethrough

newtype Html5 = Html5 {unHtml5 :: Html ()}
  deriving (Show, Semigroup, Monoid)

instance IsInline Html5 where
  lineBreak = br_ [] <> "\n"
  softBreak = "\n"
  str t = toHtml t
  entity t
    | illegalCodePoint t = toHtml ("\xFFFD" :: Text)
    | otherwise = toHtmlRaw t
  escapedChar c = toHtml (T.singleton c)
  emph ils = em_ ils
  strong ils = strong_ ils
  link target title ils = a_ (href_ (escapeURI target) :
                              [title_ title | not (T.null title)])
                              ils
  image target title ils = img_ ([src_ (escapeURI target),
                                  alt_ (renderAlt ils)] ++
                                 [title_ title | not (T.null title)])
  code t = code_ (toHtml t)
  rawInline f t
    | f == Format "html" = toHtmlRaw t
    | otherwise          = mempty

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
  paragraph ils = p_ ils <> nl
  plain ils = ils <> nl
  thematicBreak = hr_ [] <> nl
  blockQuote bs = blockquote_ (nl <> bs) <> nl
  codeBlock info t = pre_ (with code_ attr (toHtml t)) <> nl
    where attr = [class_ ("language-" <> lang) | not (T.null info)]
          lang = T.takeWhile (not . isSpace) info
  header level ils = h ils <> nl
    where h = case level of
                   1 -> h1_
                   2 -> h2_
                   3 -> h3_
                   4 -> h4_
                   5 -> h5_
                   6 -> h6_
                   _ -> p_
  rawBlock f t
    | f == Format "html" = toHtmlRaw t
    | otherwise          = mempty
  referenceLinkDefinition _ _ = mempty
  list (BulletList _) lSpacing items = ul_
    (nl <> mconcat (map li items)) <> nl
   where li x = if lSpacing == TightList
                   then li_ x <> nl
                   else li_ (nl <> x) <> nl
  list (OrderedList startnum _) lSpacing items = with ol_ attr
    (nl <> mconcat (map li items)) <> nl
   where li x = if lSpacing == TightList
                   then li_ x <> nl
                    else li_ (nl <> x) <> nl
         attr = [start_ (T.pack (show startnum)) | startnum /= 1]

nl :: Html ()
nl = toHtmlRaw ("\n" :: Text)

newtype RangedHtml5 = RangedHtml5 {unRangedHtml5 :: Html ()}
  deriving (Show, Semigroup, Monoid)

instance IsInline RangedHtml5 where
  lineBreak = RangedHtml5 $ br_ [] <> "\n"
  softBreak = RangedHtml5 "\n"
  str t = RangedHtml5 $ span_ $ toHtml t
  entity t
    | illegalCodePoint t = RangedHtml5 $ span_ $ toHtml ("\xFFFD" :: Text)
    | otherwise = RangedHtml5 $ span_ $ toHtmlRaw t
  escapedChar c = RangedHtml5 $ span_ $ toHtml (T.singleton c)
  emph ils = RangedHtml5 $ em_ $ unRangedHtml5 ils
  strong ils = RangedHtml5 $ strong_ $ unRangedHtml5 ils
  link target title ils = RangedHtml5 $
    a_ (href_ (escapeURI target) : [title_ title | not (T.null title)])
    $ unRangedHtml5 ils
  image target title ils = RangedHtml5 $
    img_ ([src_ (escapeURI target), alt_ (renderAlt $ unRangedHtml5 ils)] ++
                                   [title_ title | not (T.null title)])
  code t = RangedHtml5 $ code_ (toHtml t)
  rawInline f t
    | f == Format "html" = RangedHtml5 $ span_ $ toHtmlRaw t
    | otherwise          = mempty

instance IsBlock RangedHtml5 RangedHtml5 where
  paragraph ils = RangedHtml5 $ p_ (unRangedHtml5 ils) <> nl
  plain ils = RangedHtml5 $ unRangedHtml5 ils <> nl
  thematicBreak = RangedHtml5 $ hr_ [] <> nl
  blockQuote bs = RangedHtml5 $ blockquote_ (nl <> unRangedHtml5 bs) <> nl
  codeBlock info t = RangedHtml5 $ pre_ (with code_ attr (toHtml t)) <> nl
    where attr = [class_ ("language-" <> lang) | not (T.null info)]
          lang = T.takeWhile (not . isSpace) info
  header level ils = RangedHtml5 $ h (unRangedHtml5 ils) <> nl
    where h = case level of
                   1 -> h1_
                   2 -> h2_
                   3 -> h3_
                   4 -> h4_
                   5 -> h5_
                   6 -> h6_
                   _ -> p_
  rawBlock f t
    | f == Format "html" = RangedHtml5 $ toHtmlRaw t
    | otherwise          = mempty
  referenceLinkDefinition _ _ = mempty
  list (BulletList _) lSpacing items = RangedHtml5 $ ul_
    (nl <> mconcat (map (li . unRangedHtml5) items)) <> nl
   where li x = if lSpacing == TightList
                   then li_ x <> nl
                   else li_ (nl <> x) <> nl
  list (OrderedList startnum _) lSpacing items = RangedHtml5 $ with ol_ attr
    (nl <> mconcat (map (li . unRangedHtml5) items)) <> nl
   where li x = if lSpacing == TightList
                   then li_ x <> nl
                    else li_ (nl <> x) <> nl
         attr = [start_ (T.pack (show startnum)) | startnum /= 1]

instance Rangeable RangedHtml5 where
  ranged r (RangedHtml5 x) =
    RangedHtml5 $ with x [data_ "sourcepos" (T.pack (show r))]

instance Rangeable (Html ()) where
  ranged _ x = x


instance HasMath (Html ()) where
  inlineMath t = span_ [class_ ("math inline")] ("\\(" <> toHtml t <> "\\)")
  displayMath t = span_ [class_ ("math display")] ("\\[" <> toHtml t <> "\\]")

instance HasMath RangedHtml5 where
  inlineMath t = RangedHtml5 (inlineMath t)
  displayMath t = RangedHtml5 (displayMath t)


instance HasPipeTable (Html ()) (Html ()) where
  pipeTable aligns headerCells rows = do
    let alignToAttr LeftAlignedCol    = [style_ "text-align: left;"]
        alignToAttr CenterAlignedCol  = [style_ "text-align: center;"]
        alignToAttr RightAlignedCol   = [style_ "text-align: right;"]
        alignToAttr DefaultAlignedCol = []
    let toCell constructor align cell = do
          with constructor (alignToAttr align) cell
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
  pipeTable aligns headerCells rows =
    RangedHtml5 $ pipeTable aligns (map unRangedHtml5 headerCells)
                   (map (map unRangedHtml5) rows)

instance HasStrikethrough (Html ()) where
  strikethrough x = del_ x

instance HasStrikethrough RangedHtml5 where
  strikethrough (RangedHtml5 x) = RangedHtml5 (del_ x)

