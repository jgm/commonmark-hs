{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Commonmark.Extensions.Subscript
  ( HasSubscript(..)
  , subscriptSpec )
where
import Commonmark.Types
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.Html
import Commonmark.Nodes
import Data.Typeable (Typeable)

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

data NodeTypeSubscript a
  = NodeSubscript (Nodes a)
  deriving (Show)

instance (Typeable a, Monoid a, HasAttributes a, Rangeable a) => NodeType NodeTypeSubscript a where
  type FromNodeType NodeTypeSubscript a = HasSubscript a
  fromNodeType = \case
    NodeSubscript x -> subscript (fromNodes x)

instance ToPlainText (NodeTypeSubscript a) where
  toPlainText = \case
    NodeSubscript x -> toPlainText x

instance (Typeable a, HasSubscript a, Monoid a, HasAttributes a, Rangeable a) => HasSubscript (Nodes a) where
  subscript x = singleNode $ NodeSubscript x
