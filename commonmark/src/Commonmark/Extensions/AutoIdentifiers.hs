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
import Commonmark.Syntax
import Commonmark.Blocks
import Data.Char (isSpace, toLower, isAlphaNum,
                  generalCategory, GeneralCategory(NonSpacingMark,
                  SpacingCombiningMark, EnclosingMark, ConnectorPunctuation))
import Data.Dynamic
import qualified Data.Map as M
import qualified Data.Text as T
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
        contents <- runInlineParser
                    (removeIndent . mconcat . reverse . blockLines $ bd)
        let ident = makeIdentifier (toPlainText contents)
        counterMap <- counters <$> getState
        let key = "identifier:" <> ident
        cnt <- case M.lookup key counterMap of
                    Nothing -> return 0
                    Just x  -> return (fromDyn x (0 :: Int) + 1)
        let ident' = if cnt == 0
                        then ident
                        else ident <> "-" <> T.pack (show cnt)
        updateState $ \st ->
          st{ counters = M.insert key (toDyn cnt) counterMap }
        return $ bd{ blockAttributes = ("id",ident') : blockAttributes bd }
      Just ident -> do
        let key = "identifier:" <> ident
        counterMap <- counters <$> getState
        case M.lookup key counterMap of
          Nothing -> updateState $ \st ->
                       st{ counters = M.insert key (toDyn (1 :: Int))
                                          (counters st) }
          Just _  -> return ()
        return bd
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
