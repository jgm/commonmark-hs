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
import Data.Char (isAlpha, isLower, isSpace, isUpper, toLower, isAlphaNum,
                  generalCategory, GeneralCategory(NonSpacingMark,
                  SpacingCombiningMark, EnclosingMark, ConnectorPunctuation))
import Data.Dynamic
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Tree
import Data.Traversable
import Control.Monad (mzero, guard, void)
import Text.Parsec

autoIdentifiersSpec :: (Monad m, IsBlock il bl, IsInline il, ToPlainText il)
                    => SyntaxSpec m il bl
autoIdentifiersSpec = mempty
  { syntaxFinalParsers = [addAutoIdentifiers]
  }

-- Go through the node stack and add identifiers where they
-- are missing.
addAutoIdentifiers :: (Monad m, IsBlock il bl, IsInline il, ToPlainText il)
                   => BlockParser m il bl bl
addAutoIdentifiers = do
  nodes <- nodeStack <$> getState
  nodes' <- mapM (traverse addId) nodes
  updateState $ \st -> st{ nodeStack = nodes' }
  return mempty

addId :: (Monad m, IsBlock il bl, IsInline il, ToPlainText il)
       => BlockData m il bl -> BlockParser m il bl (BlockData m il bl)
addId bd
  | blockType (blockSpec bd) `elem` ["ATXHeading", "SetextHeading"] = do
    case lookup "id" (blockAttributes bd) of
      Nothing  -> do
        heading <- runInlineParser
                    (removeIndent . mconcat . reverse . blockLines $ bd)
        let ident = makeIdentifier (toPlainText heading)
        counterMap <- counters <$> getState
        let key = "identifier:" <> ident
        count <- case M.lookup key counterMap of
                    Nothing -> return 0
                    Just x  -> return (fromDyn x (0 :: Int) + 1)
        let ident' = if count == 0
                        then ident
                        else ident <> "-" <> T.pack (show count)
        updateState $ \st ->
          st{ counters = M.insert key (toDyn count) counterMap }
        return $ bd{ blockAttributes = ("id",ident') : blockAttributes bd }
      Just _   -> return bd
  | otherwise = return bd

makeIdentifier :: T.Text -> T.Text
makeIdentifier = toIdent
  where
    toIdent = filterPunct . spaceToDash . T.map toLower
    spaceToDash = T.map (\c -> if isSpace c then '-' else c)
    filterPunct = T.filter (\c -> isSpace c || isAlphaNum c || isAllowedPunct c)
    isAllowedPunct c = c == '-' || c == '_' ||
          generalCategory c `elem` [NonSpacingMark, SpacingCombiningMark,
                                    EnclosingMark, ConnectorPunctuation]
