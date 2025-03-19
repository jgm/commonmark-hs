{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Commonmark.Nodes
  ( Nodes (..)
  , singleNode
  , fromNodes
  , Node (..)
  , getNodeType
  , SomeNodeType (..)
  , NodeType (..)
  , NodeTypeBlock (..)
  , NodeTypeInline (..)
  , -- * Helpers
    mapNodes
  , traverseNodes
  , concatMapNodes
  )
where

import           Commonmark.Types
import           Data.Kind (Constraint)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable        (Typeable, cast)

-- | Nodes parsed from a markdown document, that can later be rendered
-- to a value of type @a@ with 'fromNodes'.
--
-- An example filtering out all raw HTML content:
--
-- > let isRawHTML node
-- >       | Just NodeRawInline{} <- getNodeType node = True
-- >       | Just NodeRawBlock{} <- getNodeType node = True
-- >       | otherwise = False
-- > in Nodes . filter (not . isRawHTML) . unNodes
newtype Nodes a = Nodes { unNodes :: [Node a] }
  deriving (Show, Semigroup, Monoid)

instance HasAttributes (Nodes a) where
  addAttributes attrs = Nodes . map (\node -> node{nodeAttributes = nodeAttributes node <> attrs}) . unNodes

instance ToPlainText (Nodes a) where
  toPlainText = foldMap toPlainText . unNodes

instance Rangeable (Nodes a) where
  ranged sr = Nodes . map (\node -> node{nodeRange = Just sr}) . unNodes

singleNode :: (NodeType node a, FromNodeType node a) => node a -> Nodes a
singleNode nodeType = Nodes [Node (SomeNodeType nodeType) [] Nothing]

fromNodes :: forall a. (Monoid a, HasAttributes a, Rangeable a) => Nodes a -> a
fromNodes = foldMap fromNode . unNodes
  where
    fromNode :: Node a -> a
    fromNode Node{nodeType = SomeNodeType nodeType, ..} =
      maybe id ranged nodeRange
        . addAttributes nodeAttributes
        $ fromNodeType nodeType

mapNodes :: (Node a -> Node a) -> Nodes a -> Nodes a
mapNodes f = Nodes . map f . unNodes

traverseNodes :: Applicative f => (Node a -> f (Node a)) -> Nodes a -> f (Nodes a)
traverseNodes f = fmap Nodes . traverse f . unNodes

concatMapNodes :: (Node a -> [Node a]) -> Nodes a -> Nodes a
concatMapNodes f = Nodes . concatMap f . unNodes

data Node a = Node
  { nodeType :: SomeNodeType a
  , nodeAttributes :: Attributes
  , nodeRange :: Maybe SourceRange
  }
  deriving (Show)

instance ToPlainText (Node a) where
  toPlainText Node{nodeType = SomeNodeType nodeType, ..} =
    case toPlainText nodeType of
      "" | Just alt <- lookup "alt" nodeAttributes -> alt
      t -> t

getNodeType :: NodeType node a => Node a -> Maybe (node a)
getNodeType = fromSomeNodeType . nodeType

data SomeNodeType a = forall node. (NodeType node a, FromNodeType node a) => SomeNodeType (node a)

instance Show (SomeNodeType a) where
  show (SomeNodeType a) = show a

class (Typeable (node a), Show (node a), ToPlainText (node a)) => NodeType node a where
  type FromNodeType node a :: Constraint

  fromSomeNodeType :: SomeNodeType a -> Maybe (node a)
  fromSomeNodeType (SomeNodeType node) = cast node

  fromNodeType :: FromNodeType node a => node a -> a

data (NodeTypeInline a)
  = NodeLineBreak
  | NodeSoftBreak
  | NodeStr Text
  | NodeEntity Text
  | NodeEscapedChar Char
  | NodeEmph (Nodes a)
  | NodeStrong (Nodes a)
  | NodeLink Text Text (Nodes a)
  | NodeImage Text Text (Nodes a)
  | NodeCode Text
  | NodeRawInline Format Text
  deriving (Show)

instance Typeable a => NodeType NodeTypeInline a where
  type FromNodeType NodeTypeInline a = IsInline a
  fromNodeType = \case
    NodeLineBreak -> lineBreak
    NodeSoftBreak -> softBreak
    NodeStr t -> str t
    NodeEntity t -> entity t
    NodeEscapedChar c -> escapedChar c
    NodeEmph nodes -> emph (fromNodes nodes)
    NodeStrong nodes -> strong (fromNodes nodes)
    NodeLink target title nodes -> link target title (fromNodes nodes)
    NodeImage target title nodes -> image target title (fromNodes nodes)
    NodeCode t -> code t
    NodeRawInline fmt t -> rawInline fmt t

instance ToPlainText (NodeTypeInline a) where
  toPlainText = \case
    NodeLineBreak -> T.singleton '\n'
    NodeSoftBreak -> T.singleton '\n'
    NodeStr t -> t
    NodeEntity t -> t
    NodeEscapedChar c -> T.singleton c
    NodeEmph nodes -> toPlainText nodes
    NodeStrong nodes -> toPlainText nodes
    NodeLink _ _ nodes -> toPlainText nodes
    NodeImage _ _ nodes -> toPlainText nodes
    NodeCode t -> t
    NodeRawInline _ _ -> mempty

instance (Typeable a, IsInline a) => IsInline (Nodes a) where
  lineBreak = singleNode NodeLineBreak
  softBreak = singleNode NodeSoftBreak
  str t = singleNode $ NodeStr t
  entity t = singleNode $ NodeEntity t
  escapedChar c = singleNode $ NodeEscapedChar c
  emph nodes = singleNode $ NodeEmph nodes
  strong nodes = singleNode $ NodeStrong nodes
  link target title nodes = singleNode $ NodeLink target title nodes
  image target title nodes = singleNode $ NodeImage target title nodes
  code t = singleNode $ NodeCode t
  rawInline f t = singleNode $ NodeRawInline f t

data NodeTypeBlock a
  = NodeParagraph (Nodes a)
  | NodePlain (Nodes a)
  | NodeThematicBreak
  | NodeBlockQuote (Nodes a)
  | NodeCodeBlock Text Text
  | NodeHeading Int (Nodes a)
  | NodeRawBlock Format Text
  | NodeReferenceLinkDefinition Text (Text, Text)
  | NodeList ListType ListSpacing [Nodes a]
  deriving (Show)

instance Typeable a => NodeType NodeTypeBlock a where
  type FromNodeType NodeTypeBlock a = IsBlock a a
  fromNodeType = \case
    NodeParagraph nodes -> paragraph (fromNodes nodes)
    NodePlain nodes -> plain (fromNodes nodes)
    NodeThematicBreak -> thematicBreak
    NodeBlockQuote nodes -> blockQuote (fromNodes nodes)
    NodeCodeBlock info t -> codeBlock info t
    NodeHeading num nodes -> heading num (fromNodes nodes)
    NodeRawBlock fmt t -> rawBlock fmt t
    NodeReferenceLinkDefinition lab dest -> referenceLinkDefinition lab dest
    NodeList lType lSpacing nodesList -> list lType lSpacing (map fromNodes nodesList)

instance ToPlainText (NodeTypeBlock a) where
  toPlainText = \case
    NodeParagraph nodes -> toPlainText nodes
    NodePlain nodes -> toPlainText nodes
    NodeThematicBreak -> mempty
    NodeBlockQuote nodes -> toPlainText nodes
    NodeCodeBlock _ t -> t
    NodeHeading _ nodes -> toPlainText nodes
    NodeRawBlock _ _ -> mempty
    NodeReferenceLinkDefinition _ _ -> mempty
    NodeList _ _ nodesList -> T.unlines $ map toPlainText nodesList

instance (Typeable a, IsBlock a a) => IsBlock (Nodes a) (Nodes a) where
  paragraph nodes = singleNode $ NodeParagraph nodes
  plain nodes = singleNode $ NodePlain nodes
  thematicBreak = singleNode $ NodeThematicBreak
  blockQuote nodes = singleNode $ NodeBlockQuote nodes
  codeBlock info t = singleNode $ NodeCodeBlock info t
  heading level nodes = singleNode $ NodeHeading level nodes
  rawBlock f t = singleNode $ NodeRawBlock f t
  referenceLinkDefinition target dest = singleNode $ NodeReferenceLinkDefinition target dest
  list lType lSpacing nodesList = singleNode $ NodeList lType lSpacing nodesList
