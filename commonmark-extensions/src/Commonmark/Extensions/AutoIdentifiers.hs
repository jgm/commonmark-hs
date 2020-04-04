{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Commonmark.Extensions.AutoIdentifiers
  ( autoIdentifiersSpec
  )
where
import Commonmark.Types
import Commonmark.Syntax
import Commonmark.Blocks
import Text.Emoji (emojis)
import Data.Char (isSpace, toLower, isAlphaNum,
                  generalCategory, GeneralCategory(NonSpacingMark,
                  SpacingCombiningMark, EnclosingMark, ConnectorPunctuation))
import Data.Dynamic
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Parsec
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

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
  return $! mempty

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
                    Just x  -> return $! (fromDyn x (0 :: Int) + 1)
        let ident' = if cnt == 0
                        then ident
                        else ident <> "-" <> T.pack (show cnt)
        updateState $ \st ->
          st{ counters = M.insert key (toDyn cnt) counterMap }
        return $! bd{ blockAttributes = ("id",ident') : blockAttributes bd }
      Just _ -> return $! bd
  | otherwise = return $! bd

makeIdentifier :: T.Text -> T.Text
makeIdentifier = toIdent
  where
    toIdent = T.concatMap
       (\c -> if isAlphaNum c || isAllowedPunct c
                 then T.singleton (toLower c)
                 else if isSpace c
                      then "-"
                      else case M.lookup (T.singleton c) emojiAliasMap of
                              Just t' -> t'
                              Nothing -> mempty)
    isAllowedPunct c = c == '-' || c == '_' ||
          generalCategory c `elem` [NonSpacingMark, SpacingCombiningMark,
                                    EnclosingMark, ConnectorPunctuation]

emojiAliasMap :: M.Map T.Text T.Text
emojiAliasMap = M.fromList $ map (\(x,y) -> (y,x)) emojis
