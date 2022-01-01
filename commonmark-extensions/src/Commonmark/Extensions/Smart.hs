{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
module Commonmark.Extensions.Smart
  ( HasQuoted(..)
  , smartPunctuationSpec )
where

import Commonmark.Types
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.Html
import Commonmark.SourceMap
import Commonmark.TokParsers (symbol)
import Text.Parsec

class IsInline il => HasQuoted il where
  singleQuoted :: il -> il
  doubleQuoted :: il -> il

instance Rangeable (Html a) => HasQuoted (Html a) where
  singleQuoted x = htmlText "‘" <> x <> htmlText "’"
  doubleQuoted x = htmlText "“" <> x <> htmlText "”"

instance (HasQuoted i, Monoid i, Semigroup i)
        => HasQuoted (WithSourceMap i) where
  singleQuoted x = (singleQuoted <$> x) <* addName "singleQuoted"
  doubleQuoted x = (doubleQuoted <$> x) <* addName "doubleQuoted"

smartPunctuationSpec :: (Monad m, IsBlock il bl, IsInline il, HasQuoted il)
                     => SyntaxSpec m il bl
smartPunctuationSpec = mempty
  { syntaxFormattingSpecs = [singleQuotedSpec, doubleQuotedSpec]
  , syntaxInlineParsers = [pEllipses, pDash]
  }

singleQuotedSpec :: (IsInline il, HasQuoted il) => FormattingSpec il
singleQuotedSpec = FormattingSpec '\'' False False (Just singleQuoted) Nothing '’'

doubleQuotedSpec :: (IsInline il, HasQuoted il) => FormattingSpec il
doubleQuotedSpec = FormattingSpec '"' False False (Just doubleQuoted) Nothing '“'

pEllipses :: (Monad m, IsInline a) => InlineParser m a
pEllipses = try $ do
  count 3 (symbol '.')
  return $! str "…"

pDash :: (Monad m, IsInline a) => InlineParser m a
pDash = try $ do
  symbol '-'
  numhyphens <- (+1) . length <$> many1 (symbol '-')
  let (emcount, encount) =
        case numhyphens of
             n | n `mod` 3 == 0 -> (n `div` 3, 0)
               | n `mod` 2 == 0 -> (0, n `div` 2)
               | n `mod` 3 == 2 -> ((n - 2) `div` 3, 1)
               | otherwise      -> ((n - 4) `div` 3, 2)
  return $! mconcat $
    replicate emcount (str "—") <>
    replicate encount (str "–")
