{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MonoLocalBinds             #-}
module Commonmark.SourceMap
  ( SourceMap(..)
  , WithSourceMap(..)
  , runWithSourceMap
  , addName
  )
where
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Map.Strict      as M
import qualified Data.Sequence as Seq
import Commonmark.Types
import Control.Monad.Trans.State

-- | A map from source positions to a pair of sequences:
-- first, elements that start at that position; then, elements
-- that end at that position.
newtype SourceMap =
  SourceMap { unSourceMap :: M.Map SourcePos (Seq.Seq Text, Seq.Seq Text) }
  deriving (Show)

instance Semigroup SourceMap where
  (SourceMap m1) <> (SourceMap m2) =
    SourceMap (M.unionWith combine m1 m2)

instance Monoid SourceMap where
  mempty = SourceMap mempty
  mappend = (<>)

instance HasAttributes (WithSourceMap a) where
  addAttributes _attrs x = x


combine :: (Seq.Seq Text, Seq.Seq Text)
        -> (Seq.Seq Text, Seq.Seq Text)
        -> (Seq.Seq Text, Seq.Seq Text)
combine (s1,e1) (s2,e2) = (s1 <> s2, e1 <> e2)

-- | Use this when you want to extract a source map as well
-- as the parsed content.
newtype WithSourceMap a =
        WithSourceMap { unWithSourceMap :: State (Maybe Text, SourceMap) a }
        deriving (Functor, Applicative, Monad)

instance (Show a, Semigroup a) => Semigroup (WithSourceMap a) where
  (WithSourceMap x1) <> (WithSourceMap x2) =
    WithSourceMap ((<>) <$> x1 <*> x2)

instance (Show a, Semigroup a, Monoid a) => Monoid (WithSourceMap a) where
  mempty = WithSourceMap (return mempty)
  mappend = (<>)

instance (Show a, Monoid a) => Show (WithSourceMap a) where
  show (WithSourceMap x) = show $ evalState x mempty

-- | Extract a parsed value and a source map from a
-- 'WithSourceMap'.
runWithSourceMap :: (Show a, Monoid a)
                 => WithSourceMap a -> (a, SourceMap)
runWithSourceMap (WithSourceMap x) = (v, sm)
  where (v, (_,sm)) = runState x (mempty, mempty)

addName :: Text -> WithSourceMap ()
addName name =
  WithSourceMap $ modify (\(_,sm) -> (Just name,sm))

instance (IsInline a, Semigroup a) => IsInline (WithSourceMap a) where
  lineBreak = lineBreak <$ addName "lineBreak"
  softBreak = softBreak <$ addName "softBreak"
  str t = str t <$ addName "str"
  entity t = entity t <$ addName "str"
  escapedChar c = escapedChar c <$ addName "escapedChar"
  emph x = (emph <$> x) <* addName "emph"
  strong x = (strong <$> x) <* addName "strong"
  link dest tit x = (link dest tit <$> x) <* addName "link"
  image dest tit x = (image dest tit <$> x) <* addName "image"
  code t = code t <$ addName "code"
  rawInline f t = rawInline f t <$ addName "rawInline"

instance (IsBlock b a, IsInline b, IsInline (WithSourceMap b), Semigroup a)
         => IsBlock (WithSourceMap b) (WithSourceMap a) where
  paragraph x = (paragraph <$> x) <* addName "paragraph"
  plain x = (plain <$> x) <* addName "plain"
  thematicBreak = thematicBreak <$ addName "thematicBreak"
  blockQuote x = (blockQuote <$> x) <* addName "blockQuote"
  codeBlock i t = codeBlock i t <$ addName "codeBlock"
  heading lev x = (heading lev <$> x) <*
                     addName ("heading" <> T.pack (show lev))
  rawBlock f t = rawBlock f t <$ addName "rawBlock"
  referenceLinkDefinition k x = referenceLinkDefinition k x <$
               addName "referenceLinkDefinition"
  list lt ls items = (list lt ls <$> sequence items) <* addName "list"

instance (Rangeable a, Monoid a, Show a)
         => Rangeable (WithSourceMap a) where
  ranged (SourceRange rs) (WithSourceMap x) =
    WithSourceMap $
      do res <- x
         (mbt, SourceMap sm) <- get
         case mbt of
           Just t -> do
             let (starts, ends) = unzip rs
             let addStart = M.alter (\case
                                       Nothing    ->
                                         Just (Seq.singleton t, mempty)
                                       Just (s,e) ->
                                         Just (t Seq.<| s, e))
             let addEnd = M.alter (\case
                                     Nothing    ->
                                       Just (mempty, Seq.singleton t)
                                     Just (s,e) ->
                                       Just (s, e Seq.|> t))
             let sm' = foldr addStart sm starts
             let sm'' = foldr addEnd sm' ends
             put (mempty, SourceMap sm'')
             return $! res
           Nothing -> return $! res

instance ToPlainText a => ToPlainText (WithSourceMap a) where
  toPlainText (WithSourceMap x) =
    let v = evalState x (mempty, mempty)
    in  toPlainText v
