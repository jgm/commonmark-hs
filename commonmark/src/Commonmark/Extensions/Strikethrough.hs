{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.Strikethrough
  ( HasStrikethrough(..)
  , strikethroughSpec )
where
import Commonmark.Types
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.Html

strikethroughSpec :: (Monad m, IsBlock il bl, IsInline il, HasStrikethrough il)
              => SyntaxSpec m il bl
strikethroughSpec = SyntaxSpec
  { syntaxBlockSpecs = []
  , syntaxBracketedSpecs = []
  , syntaxFormattingSpecs = [
      FormattingSpec '~' False Nothing (Just strikethrough) '~'
      ]
  , syntaxInlineParsers = []
  , syntaxFinalParsers = []
  }

class HasStrikethrough a where
  strikethrough :: a -> a

instance HasStrikethrough (Html a) where
  strikethrough x = htmlInline "del" (Just x)

instance (HasStrikethrough i, Monoid i)
        => HasStrikethrough (WithSourceMap i) where
  strikethrough x = (strikethrough <$> x) <* addName "strikethrough"
