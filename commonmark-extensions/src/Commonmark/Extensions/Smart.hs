{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.Smart
  ( smartPunctuationSpec )
where

import Commonmark.Types
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.TokParsers (symbol)
import Text.Parsec
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

smartPunctuationSpec :: (Monad m, IsBlock il bl, IsInline il)
                     => SyntaxSpec m il bl
smartPunctuationSpec = mempty
  { syntaxFormattingSpecs = [singleQuotedSpec, doubleQuotedSpec]
  , syntaxInlineParsers = [pEllipses, pDash]
  }

singleQuotedSpec :: IsInline il => FormattingSpec il
singleQuotedSpec = FormattingSpec '\'' False False (Just singleQuoted) Nothing '’'

doubleQuotedSpec :: IsInline il => FormattingSpec il
doubleQuotedSpec = FormattingSpec '"' False False (Just doubleQuoted) Nothing '“'

singleQuoted :: IsInline il => il -> il
singleQuoted x = str "‘" <> x <> str "’"

doubleQuoted :: IsInline il => il -> il
doubleQuoted x = str "“" <> x <> str "”"

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
