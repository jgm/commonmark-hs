{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.DefinitionList
  ( definitionListSpec
  , HasDefinitionList(..)
  )
where
import Commonmark.Tokens
import Commonmark.Types
import Commonmark.Syntax
import Commonmark.Blocks
import Commonmark.SourceMap
import Commonmark.Util
import Control.Monad (mzero, when)
import Data.Semigroup (Semigroup)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif
import Data.Dynamic
import Data.Tree
import Text.Parsec
import Data.Text.Lazy.Builder (Builder)

definitionListSpec :: (Monad m, Typeable m, IsBlock il bl, IsInline il,
                       Typeable il, Typeable bl, HasDefinitionList il bl)
                   => SyntaxSpec m il bl
definitionListSpec = SyntaxSpec
  { syntaxBlockSpecs = [definitionListItemBlockSpec]
  , syntaxBracketedSpecs = []
  , syntaxFormattingSpecs = []
  , syntaxInlineParsers = []
  , syntaxFinalParsers = []
  }

definitionListBlockSpec :: (Monad m, IsBlock il bl, HasDefinitionList il bl)
                        => BlockSpec m il bl
definitionListBlockSpec = BlockSpec
     { blockType           = "DefinitionList"
     , blockStart          = mzero
     , blockCanContain     = \sp -> blockType sp == "DefinitionListItem"
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \n -> (,n) <$> getPosition
     , blockConstructor    = undefined
     , blockFinalize       = defaultFinalizer
     }

definitionListItemBlockSpec ::
   (Monad m, IsBlock il bl, IsInline il, HasDefinitionList il bl)
   => BlockSpec m il bl
definitionListItemBlockSpec = BlockSpec
     { blockType           = "DefinitionListItem"
     , blockStart          = try $ do
         (cur:_) <- nodeStack <$> getState
         pos <- getPosition
         when (blockParagraph (bspec cur)) $ do
           undefined -- TODO see setext header
           notFollowedBy blankLine
         -- TODO store inlines in data
         let linode = Node (defBlockData definitionListItemBlockSpec){
                            blockData = toDyn TightList,
                            blockStartPos = [pos] } []
         let listnode = Node (defBlockData definitionListBlockSpec){
                            blockStartPos = [pos] } []
         case blockType (bspec cur) of
              "DefinitionList"
                -> addNodeToStack linode
              _ -> addNodeToStack listnode >> addNodeToStack linode
     , blockCanContain     = \sp -> blockType sp == "DefinitionListDefinition"
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \node@(Node ndata children) -> do
         pos <- getPosition
         gobbleSpaces 4 <|> 0 <$ lookAhead blankLine
         return (pos, node)
     , blockConstructor    = undefined
     , blockFinalize       = defaultFinalizer
     {- TODO per item tight loose
          let totight (Node nd cs)
                | blockType (blockSpec nd) == "Paragraph"
                            = Node nd{ blockSpec = plainSpec } cs
                | otherwise = Node nd cs
          let childrenToTight (Node nd cs) = Node nd (map totight cs)
          let children' =
                 if ls == TightList
                    then map childrenToTight children
                    else children
      -}
     }

definitionListDefinitionBlockSpec ::
   (Monad m, IsBlock il bl, IsInline il, HasDefinitionList il bl)
   => BlockSpec m il bl
definitionListDefinitionBlockSpec = BlockSpec
     { blockType           = "DefinitionListDefinition"
     , blockStart          = undefined
     , blockCanContain     = const True
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \node@(Node ndata children) -> do
         pos <- getPosition
         gobbleSpaces 4 <|> 0 <$ lookAhead blankLine
         return (pos, node)
     , blockConstructor    = undefined
     , blockFinalize       = defaultFinalizer
     }

class IsBlock il bl => HasDefinitionList il bl | il -> bl where
  definitionList :: [(il,[bl])] -> bl

instance HasDefinitionList Builder Builder where
  definitionList items =
    "<dl>\n" <> mconcat (map definitionListItem items) <> "</dl>"

definitionListItem :: (Builder, [Builder]) -> Builder
definitionListItem (term, defns) =
  "<dt>" <> term <> "</dt>\n" <>
  mconcat (map (\defn -> "<dd>\n" <> defn <> "</dd>\n") defns)

instance (HasDefinitionList il bl, Semigroup bl, Semigroup il)
        => HasDefinitionList (WithSourceMap il) (WithSourceMap bl) where
  definitionList items = definitionList items
