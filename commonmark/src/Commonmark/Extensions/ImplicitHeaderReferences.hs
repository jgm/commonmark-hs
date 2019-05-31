{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Commonmark.Extensions.ImplicitHeaderReferences
  ( implicitHeaderReferencesSpec
  )
where
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Syntax
import Commonmark.Blocks
import Data.Dynamic
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Tree
import Data.Traversable
import Control.Monad (mzero, guard, void)
import Text.Parsec

implicitHeaderReferencesSpec
         :: (Monad m, IsBlock il bl, IsInline il)
         => SyntaxSpec m il bl
implicitHeaderReferencesSpec = mempty
  { syntaxFinalParsers = [addHeaderReferences]
  }

-- Go through the node stack and add implicit references
-- for each header.
addHeaderReferences :: (Monad m, IsBlock il bl, IsInline il)
                    => BlockParser m il bl bl
addHeaderReferences = do
  nodes <- nodeStack <$> getState
  mapM_ (traverse addHeaderRef) nodes
  return mempty

addHeaderRef :: (Monad m, IsBlock il bl, IsInline il)
             => BlockData m il bl -> BlockParser m il bl ()
addHeaderRef bd
  | blockType (blockSpec bd) `elem` ["ATXHeading", "SetextHeading"] = do
      -- update ref map
      return ()
  | otherwise = return ()
