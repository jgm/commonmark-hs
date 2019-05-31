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
  updateState $ \st ->
    st{ nodeStack = map (fmap addIds) (nodeStack st) }
  return mempty

addIds :: (Monad m, IsBlock il bl, IsInline il)
       => BlockData m il bl -> BlockData m il bl
addIds bd =
  case lookup "id" (blockAttributes bd) of
    Nothing  -> bd{ blockAttributes =
                      ("id","dummy") : blockAttributes bd }
    Just _   -> bd
