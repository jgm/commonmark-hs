{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.Subscript
  ( HasSubscript(..)
  , subscriptSpec )
where
import Commonmark.Types
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.Html

subscriptSpec :: (Monad m, IsBlock il bl, IsInline il, HasSubscript il)
              => SyntaxSpec m il bl
subscriptSpec = mempty
  { syntaxFormattingSpecs = [
      FormattingSpec '~' True True (Just subscript) Nothing '~'
      ]
  }

class HasSubscript a where
  subscript :: a -> a

instance HasSubscript (Html a) where
  subscript x = htmlInline "sub" (Just x)

instance (HasSubscript i, Monoid i)
        => HasSubscript (WithSourceMap i) where
  subscript x = (subscript <$> x) <* addName "subscript"
