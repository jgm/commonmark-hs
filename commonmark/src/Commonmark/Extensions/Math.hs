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
import Commonmark.ParserCombinators
import Commonmark.Html (escapeHtml)
import Control.Monad (msum)
import Data.Text (Text)
import Data.Semigroup (Semigroup(..))
import Data.Text.Lazy.Builder (Builder)

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

instance HasMath Builder where
  inlineMath t = "<span class=\"math inline\">" <>
    "\\(" <> escapeHtml t <> "\\)" <> "</span>"
  displayMath t = "<span class=\"math display\">" <>
    "\\[" <> escapeHtml t <> "\\]" <> "</span>"

instance (HasMath i, Monoid i) => HasMath (WithSourceMap i) where
  inlineMath t = (inlineMath t) <* addName "inlineMath"
  displayMath t = (displayMath t) <* addName "displayMath"

parseMath :: (Monad m, HasMath a) => InlineParser m a
parseMath = pDisplayMath <|> pInlineMath

isWhitespace :: Maybe Tok -> Bool
isWhitespace (Just t) = hasType Spaces t || hasType LineEnd t
isWhitespace Nothing  = True

pInlineMath :: (Monad m, HasMath a) => InlineParser m a
pInlineMath = do
  symbol '$'
  notFollowedBy whitespace
  (_, toks) <- withRaw $ some $
                  msum   [ () <$ symbol '\\' >> anyTok
                         , noneOfToks [Symbol '$']
                         ]
  guardLastToken (not . isWhitespace)
  symbol '$'
  return $ inlineMath (untokenize toks)

pDisplayMath :: (Monad m, HasMath a) => InlineParser m a
pDisplayMath = do
  count 2 $ symbol '$'
  (_, toks) <- withRaw $ some $
                  msum   [ () <$ symbol '\\' >> anyTok
                         , noneOfToks [Symbol '$']
                         ]
  count 2 $ symbol '$'
  return $ displayMath (untokenize toks)
