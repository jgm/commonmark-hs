{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Commonmark.Extensions.AutoIdentifiers
  ( autoIdentifiersSpec
  )
where
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Syntax
import Commonmark.Blocks
import Data.Dynamic
import qualified Data.Text as T
import Data.Tree
import Data.Traversable
import Control.Monad (mzero, guard, void)
import Text.Parsec

autoIdentifiersSpec :: (Monad m, IsBlock il bl, IsInline il)
                    => SyntaxSpec m il bl
autoIdentifiersSpec = mempty
  { syntaxFinalParsers = [addAutoIdentifiers]
  }

-- Go through the node stack and add identifiers where they
-- are missing.
addAutoIdentifiers :: (Monad m, IsBlock il bl, IsInline il)
                   => BlockParser m il bl bl
addAutoIdentifiers = do
  nodes <- nodeStack <$> getState
  nodes' <- mapM (traverse addId) nodes
  updateState $ \st -> st{ nodeStack = nodes' }
  return mempty

addId :: (Monad m, IsBlock il bl, IsInline il)
       => BlockData m il bl -> BlockParser m il bl (BlockData m il bl)
addId bd
  | blockType (blockSpec bd) `elem` ["ATXHeading", "SetextHeading"] =
    case lookup "id" (blockAttributes bd) of
      Nothing  -> return $
                   bd{ blockAttributes =
                        ("id","dummy") : blockAttributes bd }
      Just _   -> return bd
  | otherwise = return bd
