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
  (
  -- addAutoIdentifiers
  )
where
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Syntax
import Commonmark.Blocks
import Data.Dynamic
import qualified Data.Text as T
import Data.Tree
import Control.Monad (mzero, guard, void)
import Text.Parsec

-- addAutoIdentifiers
--              :: (Monad m, IsInline il, IsBlock il bl,
--                  HasAutoIdentifier il bl)
--              => SyntaxSpec m il bl -> SyntaxSpec m il bl
-- addAutoIdentifiers blockTypes spec =
--   spec{ syntaxBlockSpecs = syntaxBlockSpecs spec }
-- 
-- addAutoIdentifiersToBlockSpec
--              :: (Monad m, IsInline il, IsBlock il bl,
--                  HasAutoIdentifier il bl)
--              => BlockSpec m il bl -> BlockSpec m il bl
-- addAutoIdentifiersToBlockSpec spec = spec
--      { blockConstructor    = \node -> do
--          blockConstructor node >>= addAutoIdentifier
--      }
-- 

