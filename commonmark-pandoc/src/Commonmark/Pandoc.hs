{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Commonmark.Pandoc
  ( Cm(..)
  )

where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Semigroup       (Semigroup)
import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Commonmark.Types as C
import Commonmark.Extensions.Math
import Commonmark.Extensions.PipeTable
import Commonmark.Extensions.Strikethrough
import Commonmark.Extensions.DefinitionList
import Commonmark.Extensions.Footnote
import Data.Char (isSpace)
import Data.Coerce (coerce)

newtype Cm b a = Cm { unCm :: a }
  deriving (Show, Semigroup, Monoid)

instance Functor (Cm b) where
  fmap f (Cm x) = Cm (f x)

instance Rangeable (Cm b B.Inlines) => IsInline (Cm b B.Inlines) where
  lineBreak = Cm B.linebreak
  softBreak = Cm B.softbreak
  str t = Cm $ B.text (T.unpack t)
  entity t
    | illegalCodePoint t = Cm $ B.str "\xFFFD"
    | otherwise = Cm $ B.str (T.unpack t)
  escapedChar c = Cm $ B.str [c]
  emph ils = B.emph <$> ils
  strong ils = B.strong <$> ils
  link target title ils = B.link (T.unpack target) (T.unpack title) <$> ils
  image target title ils = B.image (T.unpack target) (T.unpack title) <$> ils
  code t = Cm $ B.code (T.unpack t)
  rawInline (C.Format f) t = Cm $ B.rawInline (T.unpack f) (T.unpack t)

instance Rangeable (Cm () B.Inlines) where
  ranged _r x = x

instance Rangeable (Cm SourceRange B.Inlines) where
  ranged r x = B.spanWith ("",[],[("data-pos",show r)]) <$> x

instance (Rangeable (Cm a B.Inlines),
          Rangeable (Cm a B.Blocks))
      => IsBlock (Cm a B.Inlines) (Cm a B.Blocks) where
  paragraph ils = Cm $ B.para $ unCm ils
  plain ils = Cm $ B.plain $ unCm ils
  thematicBreak = Cm B.horizontalRule
  blockQuote bs = B.blockQuote <$> bs
  codeBlock info t = Cm $ B.codeBlockWith attr (T.unpack t)
    where attr = ("", [T.unpack lang | not (T.null lang)], [])
          lang = T.takeWhile (not . isSpace) info
  heading level ils = Cm $ B.header level $ unCm ils
  rawBlock (C.Format f) t = Cm $ B.rawBlock (T.unpack f) (T.unpack t)
  referenceLinkDefinition _ _ = Cm mempty
  list (C.BulletList _) lSpacing items = Cm $ B.bulletList items'
    where items' = if lSpacing == TightList
                      then map (B.fromList . map paraToPlain . B.toList. unCm)
                           items
                      else map unCm items
          paraToPlain (Para xs) = Plain xs
          paraToPlain x = x
  list (C.OrderedList startnum _) lSpacing items =
    Cm $ B.orderedListWith attr items'
    where items' = if lSpacing == TightList
                      then map (B.fromList . map paraToPlain . B.toList. unCm)
                           items
                      else map unCm items
          paraToPlain (Para xs) = Plain xs
          paraToPlain x = x
          attr = (startnum, DefaultStyle, DefaultDelim)

instance Rangeable (Cm () B.Blocks) where
  ranged _r x = x

instance Rangeable (Cm SourceRange B.Blocks) where
  ranged r x = B.divWith ("",[],[("data-pos",show r)]) <$> x

instance HasMath (Cm b B.Inlines) where
  inlineMath t = Cm $ B.math (T.unpack t)
  displayMath t = Cm $ B.displayMath (T.unpack t)

instance HasPipeTable (Cm a B.Inlines) (Cm a B.Blocks) where
  pipeTable aligns headerCells rows =
    Cm $ B.table mempty colspecs (map (B.plain . unCm) headerCells)
                     (map (map (B.plain . unCm)) rows)
    where toPandocAlignment LeftAlignedCol = AlignLeft
          toPandocAlignment CenterAlignedCol = AlignCenter
          toPandocAlignment RightAlignedCol = AlignRight
          toPandocAlignment DefaultAlignedCol = AlignDefault
          colspecs = map (\al -> (toPandocAlignment al, 0.0)) aligns

instance (Rangeable (Cm a B.Inlines), Rangeable (Cm a B.Blocks))
  => HasDefinitionList (Cm a B.Inlines) (Cm a B.Blocks) where
  definitionList _ items =
    Cm $ B.definitionList $ map coerce items

instance HasStrikethrough (Cm a B.Inlines) where
  strikethrough ils = B.strikeout <$> ils

instance HasAttributes (Cm a B.Blocks) where
  addAttributes attrs b = fmap (addBlockAttrs attrs) <$> b

instance HasAttributes (Cm a B.Inlines) where
  addAttributes attrs il = fmap (addInlineAttrs attrs) <$> il

addBlockAttrs :: [(T.Text, T.Text)] -> Block -> Block
addBlockAttrs attrs (Header n curattrs ils) =
  Header n (addToPandocAttr attrs curattrs) ils
addBlockAttrs attrs (CodeBlock curattrs s) =
  CodeBlock (addToPandocAttr attrs curattrs) s
addBlockAttrs _attrs x = x

addInlineAttrs :: [(T.Text, T.Text)] -> Inline -> Inline
addInlineAttrs attrs (Link curattrs ils target) =
  Link (addToPandocAttr attrs curattrs) ils target
addInlineAttrs attrs (Image curattrs ils target) =
  Image (addToPandocAttr attrs curattrs) ils target
addInlineAttrs attrs (Span curattrs ils) =
  Span (addToPandocAttr attrs curattrs) ils
addInlineAttrs attrs (Code curattrs s) =
  Code (addToPandocAttr attrs curattrs) s
addInlineAttrs _attrs x = x

addToPandocAttr :: Attributes -> Attr -> Attr
addToPandocAttr attrs curattrs = (id'', classes'', kvs'')
 where
   (id', classes', kvs') = curattrs
   id'' = maybe id' T.unpack $ lookup "id" attrs
   classes'' = maybe classes' (words . T.unpack) $ lookup "class" attrs
   kvs'' = kvs' ++ [(T.unpack k, T.unpack v) | (k,v) <- attrs,
                         k /= "id", k /= "class"]

instance (Rangeable (Cm a B.Inlines), Rangeable (Cm a B.Blocks))
     => HasFootnote (Cm a B.Inlines) (Cm a B.Blocks) where
  footnote _num _lab _x = mempty
  footnoteList _xs = mempty
  footnoteRef _num _lab contents = B.note <$> contents

illegalCodePoint :: T.Text -> Bool
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
