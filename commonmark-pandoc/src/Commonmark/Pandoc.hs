{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Commonmark.Pandoc
  ( CmPandoc(..)
  , CmRangedPandoc(..)
  )

where

import Data.Semigroup       (Semigroup, (<>))
import Text.Pandoc.Definition
import Commonmark.Types
import Commonmark.Extensions.Math
import Commonmark.Extensions.PipeTable
import Commonmark.Extensions.Strikethrough
import Commonmark.Extensions.Footnote

newtype CmPandoc = CmPandoc { unCmPandoc :: Pandoc }
  deriving (Show, Semigroup, Monoid)

newtype CmRangedPandoc = CmRangedPandoc { unCmRangedPandoc :: Pandoc }
  deriving (Show, Semigroup, Monoid)

{-
instance IsInline CmPandoc where
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

instance Rangeable Html5 where
  ranged _ x = x

instance HasMath Html5 where
  inlineMath t = Html5 $
    span_ [class_ ("math inline")] ("\\(" <> toHtml t <> "\\)")
  displayMath t = Html5 $
    span_ [class_ ("math display")] ("\\[" <> toHtml t <> "\\]")

instance HasMath RangedHtml5 where
  inlineMath t = RangedHtml5 (unHtml5 $ inlineMath t)
  displayMath t = RangedHtml5 (unHtml5 $ displayMath t)

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
  pipeTable aligns headerCells rows =
    RangedHtml5 $ unHtml5 $ pipeTable aligns
      (map (Html5 . unRangedHtml5) headerCells)
      (map (map (Html5 . unRangedHtml5)) rows)

instance HasStrikethrough Html5 where
  strikethrough ils = Html5 $ del_ (unHtml5 ils)

instance HasStrikethrough RangedHtml5 where
  strikethrough (RangedHtml5 ils) = RangedHtml5 $ del_ ils
-}
