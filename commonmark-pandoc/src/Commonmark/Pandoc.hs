{-# OPTIONS_GHC -fno-warn-orphans       #-}
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

module Commonmark.Pandoc ()

where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Commonmark.Types as C
import Commonmark.Extensions.Math
import Commonmark.Extensions.PipeTable
import Commonmark.Extensions.Strikethrough
import Commonmark.Extensions.DefinitionList
import Commonmark.Extensions.Footnote
import Commonmark.Extensions.Attributes
import Data.Char (isSpace)

instance IsInline B.Inlines where
  lineBreak = B.linebreak
  softBreak = B.softbreak
  str t = B.text (T.unpack t)
  entity t
    | illegalCodePoint t = B.str "\xFFFD"
    | otherwise = B.str (T.unpack t)
  escapedChar c = B.str [c]
  emph = B.emph
  strong = B.strong
  link target title = B.link (T.unpack target) (T.unpack title)
  image target title = B.image (T.unpack target) (T.unpack title)
  code t = B.code (T.unpack t)
  rawInline (C.Format f) t = B.rawInline (T.unpack f) (T.unpack t)

instance Rangeable B.Inlines where
  ranged r = B.spanWith ("",[],[("data-pos",show r)])

instance IsBlock B.Inlines B.Blocks where
  paragraph = B.para
  plain = B.plain
  thematicBreak = B.horizontalRule
  blockQuote = B.blockQuote
  codeBlock info t = B.codeBlockWith attr (T.unpack t)
    where attr = ("", [T.unpack lang | not (T.null lang)], [])
          lang = T.takeWhile (not . isSpace) info
  heading level = B.header level
  rawBlock (C.Format f) t = B.rawBlock (T.unpack f) (T.unpack t)
  referenceLinkDefinition _ _ = mempty
  list (C.BulletList _) lSpacing items = B.bulletList items'
    where items' = if lSpacing == TightList
                      then map (B.fromList . map paraToPlain . B.toList)
                           items
                      else items
          paraToPlain (Para xs) = Plain xs
          paraToPlain x = x
  list (C.OrderedList startnum _) lSpacing items =
    B.orderedListWith attr items'
    where items' = if lSpacing == TightList
                      then map (B.fromList . map paraToPlain . B.toList)
                           items
                      else items
          paraToPlain (Para xs) = Plain xs
          paraToPlain x = x
          attr = (startnum, DefaultStyle, DefaultDelim)

instance Rangeable B.Blocks where
  ranged r = B.divWith ("",[],[("data-pos",show r)])

instance HasMath B.Inlines where
  inlineMath t = B.math (T.unpack t)
  displayMath t = B.displayMath (T.unpack t)

instance HasPipeTable B.Inlines B.Blocks where
  pipeTable aligns headerCells rows =
    B.table mempty colspecs (map B.plain headerCells)
                     (map (map B.plain) rows)
    where toPandocAlignment LeftAlignedCol = AlignLeft
          toPandocAlignment CenterAlignedCol = AlignCenter
          toPandocAlignment RightAlignedCol = AlignRight
          toPandocAlignment DefaultAlignedCol = AlignDefault
          colspecs = map (\al -> (toPandocAlignment al, 0.0)) aligns

instance HasDefinitionList B.Inlines B.Blocks where
  definitionList _ = B.definitionList

instance HasStrikethrough B.Inlines where
  strikethrough = B.strikeout

instance HasAttributes B.Blocks where
  addAttributes attrs = fmap (addAttrs attrs)

addAttrs :: [(T.Text, T.Text)] -> Block -> Block
addAttrs attrs (Header n (id',classes',kvs') ils) =
  Header n (id'',classes'',kvs'') ils
 where
   id'' = maybe id' T.unpack $ lookup "id" attrs
   classes'' = maybe classes' (words . T.unpack) $ lookup "class" attrs
   kvs'' = kvs' ++ [(T.unpack k, T.unpack v) | (k,v) <- attrs,
                         k /= "id", k /= "class"]
addAttrs _attrs x = x

instance HasFootnote B.Inlines B.Blocks where
  footnote _num _lab _x = mempty
  footnoteList _xs = mempty
  footnoteRef _num _lab = B.note

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
