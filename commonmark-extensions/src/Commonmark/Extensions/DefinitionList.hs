{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.DefinitionList
  ( definitionListSpec
  , HasDefinitionList(..)
  )
where
import Commonmark.Types
import Commonmark.Syntax
import Commonmark.Blocks
import Commonmark.SourceMap
import Commonmark.TokParsers
import Commonmark.Html
import Control.Monad (mzero)
import Data.Dynamic
import Data.Tree
import Text.Parsec

definitionListSpec :: (Monad m, IsBlock il bl, IsInline il,
                       Typeable il, Typeable bl, HasDefinitionList il bl)
                   => SyntaxSpec m il bl
definitionListSpec = mempty
  { syntaxBlockSpecs = [definitionListDefinitionBlockSpec]
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
     , blockConstructor    = \(Node bdata items) -> do
         let listType = fromDyn (blockData bdata) LooseList
         let getItem item@(Node _ ds) = do
               term <- runInlineParser (getBlockText item)
               defs <- mapM (\c -> blockConstructor (bspec c) c) ds
               return $! (term, defs)
         definitionList listType <$> mapM getItem items
     , blockFinalize       = \(Node cdata children) parent -> do
          let spacing =
                if elem LooseList
                     (map (\child ->
                            fromDyn (blockData (rootLabel child))
                              LooseList) children)
                   then LooseList
                   else TightList
          defaultFinalizer (Node cdata{ blockData = toDyn spacing } children)
                           parent
     }

definitionListItemBlockSpec ::
   (Monad m, IsBlock il bl, IsInline il, HasDefinitionList il bl)
   => BlockSpec m il bl
definitionListItemBlockSpec = BlockSpec
     { blockType           = "DefinitionListItem"
     , blockStart          = mzero
     , blockCanContain     = \sp -> blockType sp == "DefinitionListDefinition"
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \n -> (,n) <$> getPosition
     , blockConstructor    = \_ -> mzero
     , blockFinalize       = \(Node cdata children) parent -> do
         let listSpacing   = fromDyn (blockData cdata) LooseList
         let totight (Node nd cs)
               | blockType (blockSpec nd) == "Paragraph"
                           = Node nd{ blockSpec = plainSpec } cs
               | otherwise = Node nd cs
         let childrenToTight (Node nd cs) = Node nd (map totight cs)
         let children' =
                case listSpacing of
                  TightList -> map childrenToTight children
                  LooseList -> children
         defaultFinalizer (Node cdata children') parent
     }


definitionListDefinitionBlockSpec ::
   (Monad m, IsBlock il bl, IsInline il, HasDefinitionList il bl)
   => BlockSpec m il bl
definitionListDefinitionBlockSpec = BlockSpec
     { blockType           = "DefinitionListDefinition"
     , blockStart          = try $ do
         initcol <- sourceColumn <$> getPosition
         gobbleUpToSpaces 3
         pos <- getPosition
         symbol ':' <|> symbol '~'
         try (gobbleUpToSpaces 4 <* notFollowedBy whitespace)
           <|> gobbleSpaces 1
           <|> 1 <$ lookAhead lineEnd
         finalcol <- sourceColumn <$> getPosition
         (Node bdata children : rest) <- nodeStack <$> getState
         let definitionIndent :: Int
             definitionIndent = finalcol - initcol
         let defnode = Node (defBlockData
                              definitionListDefinitionBlockSpec){
                                  blockStartPos = [pos],
                                  blockData = toDyn definitionIndent } []
         if blockType (blockSpec bdata) == "DefinitionListItem"
            then addNodeToStack defnode
            else do
             linode <-
               if blockParagraph (blockSpec bdata)
                 then do
                   -- a) we're in a paragraph -> TightList
                   --    make cur a DefinitionListItem instead
                   --    keep the tokens; they will be the term
                   -- remove paragraph from stack
                   updateState $ \st -> st{ nodeStack = rest }
                   return $! Node (defBlockData definitionListItemBlockSpec)
                            { blockData = toDyn TightList
                            , blockLines = blockLines bdata
                            , blockStartPos = blockStartPos bdata
                            } []
                 else
                   case children of
                     (lastChild : rest')
                       | blockParagraph (bspec lastChild) -> do
                         -- b) previous sibling is a paragraph -> LooseList
                         --    last child of cur is a Paragraph
                         --    remove this child and mk new child with its
                         --    content and position.  tokens will be term.
                         -- remove paragraph from stack
                         updateState $ \st -> st{ nodeStack =
                              Node bdata rest' : rest }
                         return $! Node (defBlockData
                                    definitionListItemBlockSpec)
                                  { blockData = toDyn LooseList
                                  , blockStartPos = blockStartPos
                                                     (rootLabel lastChild)
                                  , blockLines = blockLines
                                        (rootLabel lastChild)
                                  } []
                     _ -> mzero

             let listnode = Node (defBlockData definitionListBlockSpec){
                                blockStartPos = blockStartPos
                                             (rootLabel linode) } []
             (Node bdata' children' : rest') <- nodeStack <$> getState
             -- if last child was DefinitionList, set that to current
             case children' of
               m:ms | blockType (blockSpec (rootLabel m)) == "DefinitionList"
                   -> updateState $ \st -> st{ nodeStack =
                        m : Node bdata' ms : rest' }
               _ -> return ()
             (Node bdata'' _ : _) <- nodeStack <$> getState
             case blockType (blockSpec bdata'') of
                  "DefinitionList"
                    -> addNodeToStack linode >> addNodeToStack defnode
                  _ -> addNodeToStack listnode >> addNodeToStack linode >>
                       addNodeToStack defnode
         return BlockStartMatch
     , blockCanContain     = const True
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \node@(Node ndata _cs) -> do
         pos <- getPosition
         let definitionIndent = fromDyn (blockData ndata) 0
         gobbleSpaces definitionIndent <|> 0 <$ lookAhead blankLine
         return $! (pos, node)
     , blockConstructor    = fmap mconcat . renderChildren
     , blockFinalize       = defaultFinalizer
     }

class IsBlock il bl => HasDefinitionList il bl | il -> bl where
  definitionList :: ListSpacing -> [(il,[bl])] -> bl

instance Rangeable (Html a) =>
         HasDefinitionList (Html a) (Html a) where
  definitionList spacing items =
    htmlBlock "dl" $ Just $ htmlRaw "\n" <>
       mconcat (map (definitionListItem spacing) items)

definitionListItem :: ListSpacing -> (Html a, [Html a]) -> Html a
definitionListItem spacing (term, defns) =
  htmlBlock "dt" (Just term) <>
   mconcat (map (\defn ->
            case spacing of
              LooseList -> htmlBlock "dd" (Just (htmlRaw "\n" <> defn))
              TightList -> htmlBlock "dd" (Just defn)) defns)

instance (HasDefinitionList il bl, Semigroup bl, Semigroup il)
        => HasDefinitionList (WithSourceMap il) (WithSourceMap bl) where
  definitionList spacing items = do
    let (terms, defs) = unzip items
    terms' <- sequence terms
    defs' <- mapM sequence defs
    let res = definitionList spacing (zip terms' defs')
    addName "definitionList"
    return res
