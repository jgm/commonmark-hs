{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}

module Commonmark.Pandoc
  ( Cm(..)
  )

where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import qualified Text.Pandoc.Builder as B
import Commonmark.Types as C
import Commonmark.Extensions.Math
import Commonmark.Extensions.Emoji
import Commonmark.Extensions.PipeTable
import Commonmark.Extensions.Strikethrough
import Commonmark.Extensions.Superscript
import Commonmark.Extensions.Subscript
import Commonmark.Extensions.DefinitionList
import Commonmark.Extensions.Attributes
import Commonmark.Extensions.Footnote
import Data.Char (isSpace)
import Data.Coerce (coerce)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup       (Semigroup)
#endif

newtype Cm b a = Cm { unCm :: a }
  deriving (Show, Semigroup, Monoid)

instance Functor (Cm b) where
  fmap f (Cm x) = Cm (f x)

instance Rangeable (Cm b B.Inlines) => IsInline (Cm b B.Inlines) where
  lineBreak = Cm B.linebreak
  softBreak = Cm B.softbreak
  str t = Cm $ B.text t
  entity t
    | illegalCodePoint t = Cm $ B.str "\xFFFD"
    | otherwise = Cm $ B.str t
  escapedChar c = Cm $ B.str $ T.singleton c
  emph ils = B.emph <$> ils
  strong ils = B.strong <$> ils
  link target title ils = B.link target title <$> ils
  image target title ils = B.image target title <$> ils
  code t = Cm $ B.code t
  rawInline (C.Format f) t = Cm $ B.rawInline f t

instance Rangeable (Cm () B.Inlines) where
  ranged _r x = x

instance Rangeable (Cm SourceRange B.Inlines) where
  ranged r = addAttributes [("data-pos", T.pack (show r))]

instance Walkable Inline b => ToPlainText (Cm a b) where
  toPlainText = stringify . unCm

instance (Rangeable (Cm a B.Inlines),
          Rangeable (Cm a B.Blocks))
      => IsBlock (Cm a B.Inlines) (Cm a B.Blocks) where
  paragraph ils = Cm $ B.para $ unCm ils
  plain ils = Cm $ B.plain $ unCm ils
  thematicBreak = Cm B.horizontalRule
  blockQuote bs = B.blockQuote <$> bs
  codeBlock info t = Cm $ B.codeBlockWith attr t
    where attr = ("", [lang | not (T.null lang)], [])
          lang = T.takeWhile (not . isSpace) info
  heading level ils = Cm $ B.header level $ unCm ils
  rawBlock (C.Format f) t = Cm $ B.rawBlock f t
  referenceLinkDefinition _ _ = Cm mempty
  list (C.BulletList _) lSpacing items = Cm $ B.bulletList items'
    where items' = if lSpacing == TightList
                      then map (B.fromList . map paraToPlain . B.toList. unCm)
                           items
                      else map unCm items
          paraToPlain (Para xs) = Plain xs
          paraToPlain x = x
  list (C.OrderedList startnum enumtype delimtype) lSpacing items =
    Cm $ B.orderedListWith attr items'
    where items' = if lSpacing == TightList
                      then map (B.fromList . map paraToPlain . B.toList. unCm)
                           items
                      else map unCm items
          paraToPlain (Para xs) = Plain xs
          paraToPlain x = x
          sty = case enumtype of
                  C.Decimal    -> B.Decimal
                  C.UpperAlpha -> B.UpperAlpha
                  C.LowerAlpha -> B.LowerAlpha
                  C.UpperRoman -> B.UpperRoman
                  C.LowerRoman -> B.LowerRoman
          delim = case delimtype of
                    C.Period    -> B.Period
                    C.OneParen  -> B.OneParen
                    C.TwoParens -> B.TwoParens
          attr = (startnum, sty, delim)

instance Rangeable (Cm () B.Blocks) where
  ranged _r x = x

instance Rangeable (Cm SourceRange B.Blocks) where
  ranged r x = B.divWith ("",[],[("data-pos",T.pack (show r))]) <$> x

instance HasMath (Cm b B.Inlines) where
  inlineMath t = Cm $ B.math t
  displayMath t = Cm $ B.displayMath t

instance HasEmoji (Cm b B.Inlines) where
  emoji kw t = Cm $ B.spanWith ("",["emoji"],[("emoji",t)])
                  $ B.text kw

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

instance Rangeable (Cm a B.Blocks)
  => HasDiv (Cm a B.Blocks) where
  div_ bs = B.divWith nullAttr <$> bs

instance HasStrikethrough (Cm a B.Inlines) where
  strikethrough ils = B.strikeout <$> ils

instance HasSuperscript (Cm a B.Inlines) where
  superscript ils = B.superscript <$> ils

instance HasSubscript (Cm a B.Inlines) where
  subscript ils = B.subscript <$> ils

instance Rangeable (Cm a B.Inlines) => HasSpan (Cm a B.Inlines) where
  spanWith attrs ils =
    B.spanWith (addToPandocAttr attrs nullAttr) <$> ils

instance HasAttributes (Cm a B.Blocks) where
  addAttributes attrs b = fmap (addBlockAttrs attrs) <$> b

instance HasAttributes (Cm a B.Inlines) where
  addAttributes attrs il = fmap (addInlineAttrs attrs) <$> il

addBlockAttrs :: [(T.Text, T.Text)] -> Block -> Block
addBlockAttrs attrs (Header n curattrs ils) =
  Header n (addToPandocAttr attrs curattrs) ils
addBlockAttrs attrs (CodeBlock curattrs s) =
  CodeBlock (addToPandocAttr attrs curattrs) s
addBlockAttrs attrs x =
  Div (addToPandocAttr attrs nullAttr) [x]

addInlineAttrs :: [(T.Text, T.Text)] -> Inline -> Inline
addInlineAttrs attrs (Link curattrs ils target) =
  Link (addToPandocAttr attrs curattrs) ils target
addInlineAttrs attrs (Image curattrs ils target) =
  Image (addToPandocAttr attrs curattrs) ils target
addInlineAttrs attrs (Span curattrs ils) =
  Span (addToPandocAttr attrs curattrs) ils
addInlineAttrs attrs (Code curattrs s) =
  Code (addToPandocAttr attrs curattrs) s
addInlineAttrs attrs x =
  Span (addToPandocAttr attrs nullAttr) [x]

addToPandocAttr :: Attributes -> Attr -> Attr
addToPandocAttr attrs curattrs = (id'', classes'', kvs'')
 where
   (id', classes', kvs') = curattrs
   id'' = fromMaybe id' $ lookup "id" attrs
   classes'' = case lookup "class" attrs of
                      Nothing  -> classes'
                      Just cs  -> classes' ++ T.words cs
   kvs'' = [(k, v) | (k, v) <- kvs' ++
                              map (\(x,y) -> (x, y)) attrs
                   , k /= "id"
                   , k /= "class"]

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

stringify :: Walkable Inline a => a -> T.Text
stringify = query go . walk (deNote . deQuote)
  where go :: Inline -> T.Text
        go Space                                         = " "
        go SoftBreak                                     = " "
        go (Str x)                                       = x
        go (Code _ x)                                    = x
        go (Math _ x)                                    = x
        go (RawInline (B.Format "html") t)
           | "<br" `T.isPrefixOf` t                      = " "
        go LineBreak                                     = " "
        go _                                             = mempty

deNote :: Inline -> Inline
deNote (Note _) = Str ""
deNote x        = x

deQuote :: Inline -> Inline
deQuote (Quoted SingleQuote xs) =
  Span ("",[],[]) (Str "\8216" : xs ++ [Str "\8217"])
deQuote (Quoted DoubleQuote xs) =
  Span ("",[],[]) (Str "\8220" : xs ++ [Str "\8221"])
deQuote x = x
