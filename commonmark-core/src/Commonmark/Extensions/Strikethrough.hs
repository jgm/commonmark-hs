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

strikethroughSpec :: (Monad m, IsBlock il bl, IsInline il, HasStrikethrough il)
              => SyntaxSpec m il bl
strikethroughSpec = SyntaxSpec
  { syntaxBlockSpecs = []
  , syntaxBracketedSpecs = []
  , syntaxFormattingSpecs = [
      FormattingSpec '~' False Nothing (Just strikethrough) '~'
      ]
  , syntaxInlineParsers = []
  }

class HasStrikethrough a where
  strikethrough :: a -> a

{-
instance HasStrikethrough (Html ()) where
  strikethrough x = del_ x

instance HasStrikethrough RangedHtml where
  strikethrough (RangedHtml x) = RangedHtml (del_ x)
-}

instance (HasStrikethrough i, Monoid i)
        => HasStrikethrough (WithSourceMap i) where
  strikethrough x = (strikethrough <$> x) <* addName "strikethrough"
