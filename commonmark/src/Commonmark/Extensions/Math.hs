{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.Math
  ( HasMath(..)
  , mathSpec )
where
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.Util
import Commonmark.Html (escapeHtml)
import Text.Parsec
import Data.Text (Text)
import Data.Semigroup (Semigroup(..))

mathSpec :: (Monad m, IsBlock il bl, IsInline il, HasMath il)
         => SyntaxSpec m il bl
mathSpec = SyntaxSpec
  { syntaxBlockSpecs = []
  , syntaxBracketedSpecs = []
  , syntaxFormattingSpecs = []
  , syntaxInlineParsers = [parseMath]
  , syntaxFinalParsers = []
  }

class HasMath a where
  inlineMath :: Text -> a
  displayMath :: Text -> a

instance HasMath Html5 where
  inlineMath t = "<span class=\"math inline\">" <>
    "\\(" <> Html5 (escapeHtml t) <> "\\)" <> "</span>"
  displayMath t = "<span class=\"math display\">" <>
    "\\[" <> Html5 (escapeHtml t) <> "\\]" <> "</span>"

instance (HasMath i, Monoid i) => HasMath (WithSourceMap i) where
  inlineMath t = (inlineMath t) <* addName "inlineMath"
  displayMath t = (displayMath t) <* addName "displayMath"

parseMath :: (Monad m, HasMath a) => InlineParser m a
parseMath = pDisplayMath <|> pInlineMath

pInlineMath :: (Monad m, HasMath a) => InlineParser m a
pInlineMath = try $ do
  symbol '$'
  notFollowedBy whitespace
  (_, toks) <- withRaw $ many1 $
                  choice [ () <$ symbol '\\' >> anyTok
                         , whitespace >> lookAhead (noneOfToks [Symbol '$'])
                         , noneOfToks [Symbol '$']
                         ]
  symbol '$'
  return $ inlineMath (untokenize toks)

pDisplayMath :: (Monad m, HasMath a) => InlineParser m a
pDisplayMath = try $ do
  count 2 $ symbol '$'
  (_, toks) <- withRaw $ many1 $
                  choice [ () <$ symbol '\\' >> anyTok
                         , noneOfToks [Symbol '$']
                         ]
  count 2 $ symbol '$'
  return $ displayMath (untokenize toks)
