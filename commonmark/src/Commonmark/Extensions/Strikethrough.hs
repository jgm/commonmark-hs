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
import Data.Semigroup (Semigroup(..))
import Data.Text.Lazy.Builder (Builder)

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

instance HasStrikethrough Builder where
  strikethrough x = "<del>" <> x <> "</del>"

instance (HasStrikethrough i, Monoid i)
        => HasStrikethrough (WithSourceMap i) where
  strikethrough x = (strikethrough <$> x) <* addName "strikethrough"
