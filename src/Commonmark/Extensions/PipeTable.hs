{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveDataTypeable    #-}

-- Note: we require pipes at the beginning and end
-- of each line, so this type of table is not supported:
--
--  a | b
--  - | -
--  1 | 2
--
-- This is for reasons of parsing efficiency; otherwise
-- we'd need to parse the whole line to tell if it's a
-- table line.
--
-- [ ] Spec this out and adjust test/pipe_table.txt.

module Commonmark.Extensions.PipeTable
 ( HasPipeTable
 , ColAlignment
 , pipeTableSpec
 )
where

import Control.Monad (guard, zipWithM_, unless)
import Commonmark.Syntax
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Util
import Commonmark.Blocks
import Text.Parsec
import Data.Dynamic
import Data.Tree
import Data.Data
import Lucid

data ColAlignment = LeftAlignedCol
                  | CenterAlignedCol
                  | RightAlignedCol
                  | DefaultAlignedCol
                  deriving (Show, Eq, Data, Typeable)

data PipeTableData = PipeTableData
     { pipeTableAlignments :: [ColAlignment]
     , pipeTableHeaders    :: [[Tok]]
     , pipeTableRows       :: [[[Tok]]] -- in reverse order
     } deriving (Show, Eq, Data, Typeable)

class HasPipeTable il bl where
  pipeTable :: [ColAlignment] -> [il] -> [[il]] -> bl

instance HasPipeTable (Html ()) (Html ()) where
  pipeTable aligns headerCells rows = do
    let alignToAttr LeftAlignedCol    = [style_ "text-align: left;"]
        alignToAttr CenterAlignedCol  = [style_ "text-align: center;"]
        alignToAttr RightAlignedCol   = [style_ "text-align: right;"]
        alignToAttr DefaultAlignedCol = []
    let toCell constructor align cell = do
          with constructor (alignToAttr align) cell
          "\n"
    table_ $ do
      "\n"
      thead_ $ do
        "\n"
        tr_ $ do
          "\n"
          zipWithM_ (toCell th_) aligns headerCells
        "\n"
      "\n"
      unless (null rows) $ do
        tbody_ $ do
          "\n"
          mapM_ ((>> "\n") . tr_ . ("\n" >>) .
                   zipWithM_ (toCell td_) aligns) rows
        "\n"
    "\n"

instance HasPipeTable RangedHtml RangedHtml where
  pipeTable aligns headerCells rows =
    RangedHtml $ pipeTable aligns (map unRangedHtml headerCells)
                   (map (map unRangedHtml) rows)

pCells :: Monad m => ParsecT [Tok] s m [[Tok]]
pCells = try $ do
  symbol '|'
  cells <- many1 (try $ pCell <* symbol '|')
  lookAhead blankLine
  return cells

pCell :: Monad m => ParsecT [Tok] s m [Tok]
pCell = mconcat <$> many1
  ( try
      (do tok' <- symbol '\\'
          tok@(Tok (Symbol c) _ _) <- anySymbol
          if c == '|'
             then return [tok]
             else return [tok',tok])
  <|> (do tok <- (satisfyTok $ \t -> not (hasType (Symbol '|') t ||
                                       hasType LineEnd t))
          return [tok])
  ) <|> ([] <$ lookAhead (symbol '|'))

pDividers :: Monad m => ParsecT [Tok] s m [ColAlignment]
pDividers = try $ do
  symbol '|'
  aligns <- many1 (try $ pDivider <* symbol '|')
  lookAhead blankLine
  return aligns

pDivider :: Monad m => ParsecT [Tok] s m ColAlignment
pDivider = try $ do
  skipMany $ satisfyTok (hasType Spaces)
  align <- choice
    [ CenterAlignedCol <$
       try (symbol ':' >> many1 (symbol '-') >> symbol ':')
    , LeftAlignedCol <$
       try (symbol ':' >> many1 (symbol '-'))
    , RightAlignedCol <$
       try (many1 (symbol '-') >> symbol ':')
    , DefaultAlignedCol <$
       many1 (symbol '-')
    ]
  skipMany $ satisfyTok (hasType Spaces)
  return align

pipeTableSpec :: (Monad m, IsBlock il bl, IsInline il, HasPipeTable il bl)
              => SyntaxSpec m il bl
pipeTableSpec = SyntaxSpec
  { syntaxBlockSpecs = [pipeTableBlockSpec]
  , syntaxBracketedSpecs = []
  , syntaxFormattingSpecs = []
  , syntaxInlineParsers = []
  }

pipeTableBlockSpec :: (Monad m, IsBlock il bl, IsInline il,
                       HasPipeTable il bl)
                   => BlockSpec m il bl
pipeTableBlockSpec = BlockSpec
     { blockType           = "PipeTable" -- :: Text
     , blockStart          = try $ do -- :: BlockParser m il bl ()
         interruptsParagraph >>= guard . not
         nonindentSpaces
         pos <- getPosition
         (cells, toks) <- withRaw pCells
         nl <- lookAhead lineEnd
         let tabledata = PipeTableData
              { pipeTableAlignments = []
              , pipeTableHeaders    = cells
              , pipeTableRows       = []
              }
         addNodeToStack $
               Node (defBlockData pipeTableBlockSpec){
                         blockStartPos = [pos]
                       , blockData = toDyn tabledata
                       , blockLines = [toks ++ [nl]]
                       } []
     , blockCanContain     = \_ -> False -- :: BlockSpec m il bl -> Bool
     , blockContainsLines  = False -- :: Bool
     , blockParagraph      = False -- :: Bool
     , blockContinue       = \(Node ndata children) -> try $ do
         nonindentSpaces
         let tabledata = fromDyn
                (blockData ndata)
                PipeTableData{ pipeTableAlignments = []
                             , pipeTableHeaders = []
                             , pipeTableRows = [] }
         pos <- getPosition
         if null (blockLines ndata)
           then do
             cells <- pCells
             let tabledata' = tabledata{ pipeTableRows =
                                 cells : pipeTableRows tabledata }
             return (pos, Node ndata{ blockData =
                                 toDyn tabledata' } children)
           else
             -- last line was first; check for separators
             -- and if not found, convert to paragraph:
             try (do aligns <- pDividers
                     guard $ length aligns ==
                             length (pipeTableHeaders tabledata)
                     let tabledata' = tabledata{ pipeTableAlignments = aligns }
                     return (pos, Node ndata{ blockLines = []
                                            , blockData = toDyn tabledata'
                                            } children))
             <|> return (pos, Node ndata{
                                 blockSpec = paraSpec } children)
     , blockConstructor    = \(Node ndata _) -> do
         let tabledata = fromDyn
                (blockData ndata)
                PipeTableData{ pipeTableAlignments = []
                             , pipeTableHeaders = []
                             , pipeTableRows = [] }
         let aligns = pipeTableAlignments tabledata
         headers <- mapM runInlineParser (pipeTableHeaders tabledata)
         let numcols = length headers
         rows <- mapM (mapM runInlineParser . take numcols . (++ (repeat [])))
                    (reverse $ pipeTableRows tabledata)
         return (pipeTable aligns headers rows)
     , blockFinalize       = defaultFinalizer
     }
