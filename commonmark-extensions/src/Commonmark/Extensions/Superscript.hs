{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.Superscript
  ( HasSuperscript(..)
  , superscriptSpec )
where
import Commonmark.Types
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.Html

superscriptSpec :: (Monad m, IsBlock il bl, IsInline il, HasSuperscript il)
              => SyntaxSpec m il bl
superscriptSpec = mempty
  { syntaxFormattingSpecs = [
      FormattingSpec '^' True True (Just superscript) Nothing '^'
      ]
  }

class HasSuperscript a where
  superscript :: a -> a

instance HasSuperscript (Html a) where
  superscript x = htmlInline "sup" (Just x)

instance (HasSuperscript i, Monoid i)
        => HasSuperscript (WithSourceMap i) where
  superscript x = (superscript <$> x) <* addName "superscript"
