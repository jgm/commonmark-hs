{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.Attributes
  ( Attributes(..)
  , HasAttributes(..)
  , headerAttributesSpec
  )
where
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.Util
import Commonmark.Blocks
import Commonmark.Html (escapeHtml, addAttribute, HtmlAttribute)
import Data.Tree
import Control.Monad (mzero)
import Text.Parsec
import Data.Text (Text)
import Data.Semigroup (Semigroup(..))

headerAttributesSpec
             :: (Monad m, IsBlock il bl, IsInline il, HasAttributes bl)
             => SyntaxSpec m il bl
headerAttributesSpec = SyntaxSpec
  { syntaxBlockSpecs = [atxHeaderWithAttributesSpec]
  , syntaxBracketedSpecs = []
  , syntaxFormattingSpecs = []
  , syntaxInlineParsers = []
  , syntaxFinalParsers = []
  }

class HasAttributes a where
  addAttributes :: Attributes -> a -> a

instance HasAttributes (Html a) where
  addAttributes attrs x = foldr addAttribute x attrs

instance HasAttributes (WithSourceMap a) where
  addAttributes _attrs x = x

type Attributes = [HtmlAttribute]

atxHeaderWithAttributesSpec
    :: (Monad m, IsBlock il bl, IsInline il, HasAttributes bl)
    => BlockSpec m il bl
atxHeaderWithAttributesSpec = atxHeaderSpec
  { blockType = "ATXHeaderWithAttributes"
  , blockStart = do
       res <- blockStart atxHeaderSpec
       nodestack <- nodeStack <$> getState
       case nodestack of
         [] -> mzero
         (Node nd cs:ns) -> updateState $ \st -> st{
              nodeStack = Node nd{ blockSpec = atxHeaderWithAttributesSpec
                                 } cs : ns }
       return res
  , blockConstructor = \node ->
      addAttributes [("test","value")] <$> blockConstructor atxHeaderSpec node
  }

