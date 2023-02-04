{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Commonmark.Extensions.Strikethrough
  ( HasStrikethrough(..)
  , strikethroughSpec )
where
import Commonmark.Types
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.Html
import Commonmark.Nodes
import Data.Typeable (Typeable)

strikethroughSpec :: (Monad m, IsBlock il bl, IsInline il, HasStrikethrough il)
              => SyntaxSpec m il bl
strikethroughSpec = mempty
  { syntaxFormattingSpecs = [
      FormattingSpec '~' True True Nothing (Just strikethrough) '~'
      ]
  }

class HasStrikethrough a where
  strikethrough :: a -> a

instance HasStrikethrough (Html a) where
  strikethrough x = htmlInline "del" (Just x)

instance (HasStrikethrough i, Monoid i)
        => HasStrikethrough (WithSourceMap i) where
  strikethrough x = (strikethrough <$> x) <* addName "strikethrough"

data NodeTypeStrikethrough a
  = NodeStrikethrough (Nodes a)
  deriving (Show)

instance (Typeable a, Monoid a, HasAttributes a, Rangeable a) => NodeType NodeTypeStrikethrough a where
  type FromNodeType NodeTypeStrikethrough a = HasStrikethrough a
  fromNodeType = \case
    NodeStrikethrough x -> strikethrough (fromNodes x)

instance ToPlainText (NodeTypeStrikethrough a) where
  toPlainText = \case
    NodeStrikethrough x -> toPlainText x

instance (Typeable a, HasStrikethrough a, Monoid a, HasAttributes a, Rangeable a) => HasStrikethrough (Nodes a) where
  strikethrough x = singleNode $ NodeStrikethrough x
