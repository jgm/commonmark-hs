{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Commonmark.Extensions.Superscript
  ( HasSuperscript(..)
  , superscriptSpec )
where
import Commonmark.Types
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.Html
import Commonmark.Nodes
import Data.Typeable (Typeable)

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

data NodeTypeSuperscript a
  = NodeSuperscript (Nodes a)
  deriving (Show)

instance (Typeable a, Monoid a, HasAttributes a, Rangeable a) => NodeType NodeTypeSuperscript a where
  type FromNodeType NodeTypeSuperscript a = HasSuperscript a
  fromNodeType = \case
    NodeSuperscript x -> superscript (fromNodes x)

instance ToPlainText (NodeTypeSuperscript a) where
  toPlainText = \case
    NodeSuperscript x -> toPlainText x

instance (Typeable a, HasSuperscript a, Monoid a, HasAttributes a, Rangeable a) => HasSuperscript (Nodes a) where
  superscript x = singleNode $ NodeSuperscript x
