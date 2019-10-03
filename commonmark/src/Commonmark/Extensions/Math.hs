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
import Commonmark.Html
import Text.Parsec
import Data.Text (Text)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))
#endif

mathSpec :: (Monad m, IsBlock il bl, IsInline il, HasMath il)
         => SyntaxSpec m il bl
mathSpec = mempty
  { syntaxInlineParsers = [parseMath]
  }

class HasMath a where
  inlineMath :: Text -> a
  displayMath :: Text -> a

instance HasMath (Html a) where
  inlineMath t = addAttribute ("class", "math inline") $
    htmlInline "span" $ Just $ htmlRaw "\\(" <> htmlText t <> htmlRaw "\\)"
  displayMath t = addAttribute ("class", "math display") $
    htmlInline "span" $ Just $ htmlRaw "\\[" <> htmlText t <> htmlRaw "\\]"

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
