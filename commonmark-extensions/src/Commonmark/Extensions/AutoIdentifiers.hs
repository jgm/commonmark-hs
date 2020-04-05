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
import Data.Char (isSpace, isAlphaNum, isAscii, isMark,
                  generalCategory, GeneralCategory(ConnectorPunctuation))
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
  { syntaxFinalParsers = [addAutoIdentifiers False]
  }

-- Go through the node stack and add identifiers where they
-- are missing.
addAutoIdentifiers :: (Monad m, IsBlock il bl, IsInline il, ToPlainText il)
                   => Bool -> BlockParser m il bl bl
addAutoIdentifiers ascii = do
  nodes <- nodeStack <$> getState
  nodes' <- mapM (traverse $ addId ascii) nodes
  updateState $ \st -> st{ nodeStack = nodes' }
  return $! mempty

addId :: (Monad m, IsBlock il bl, IsInline il, ToPlainText il)
       => Bool -> BlockData m il bl -> BlockParser m il bl (BlockData m il bl)
addId ascii bd
  | blockType (blockSpec bd) `elem` ["ATXHeading", "SetextHeading"] = do
    case lookup "id" (blockAttributes bd) of
      Nothing  -> do
        contents <- runInlineParser
                    (removeIndent . mconcat . reverse . blockLines $ bd)
        let ident = makeIdentifier ascii (toPlainText contents)
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

makeIdentifier :: Bool -> T.Text -> T.Text
makeIdentifier ascii = toIdent . T.toLower
  where
    toIdent = T.concatMap f
    f '-' = "-"
    f '_' = "_"
    f c | isSpace c = "-"
    f c | isAlphaNum c || isMark c ||
          generalCategory c == ConnectorPunctuation
                    = fromchar c
        | otherwise = mempty
    fromchar c
      | ascii
      , not (isAscii c) = mempty
      | otherwise       = T.singleton c
