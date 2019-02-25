{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveDataTypeable    #-}

module Commonmark.Extensions.PipeTable
 ( HasPipeTable(..)
 , ColAlignment(..)
 , pipeTableSpec
 )
where

import Control.Monad (guard)
import Commonmark.Syntax
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Util
import Commonmark.Blocks
import Commonmark.SourceMap
import Text.Parsec
import Data.String
import Data.Dynamic
import Data.Tree
import Data.Data
import Data.Semigroup (Semigroup(..))

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

instance HasPipeTable Html5 Html5 where
  pipeTable aligns headerCells rows =
    "<table>\n" <>
    (if null headerCells
        then ""
        else "<thead>\n" <>
             toRow "th" aligns headerCells <>
             "</thead>\n") <>
    (if null rows
        then ""
        else "<tbody>\n" <>
             mconcat (map (toRow "td" aligns) rows) <>
             "</tbody>\n") <>
    "</table>\n"
    where alignToAttr LeftAlignedCol    = " style=\"text-align: left;\""
          alignToAttr CenterAlignedCol  = " style=\"text-align: center;\""
          alignToAttr RightAlignedCol   = " style=\"text-align: right;\""
          alignToAttr DefaultAlignedCol = ""
          toRow constructor aligns' cells =
            "<tr>\n" <> mconcat (zipWith (toCell constructor) aligns' cells)
              <> "</tr>\n"
          toCell constructor align cell =
            "<" <> fromString constructor <> alignToAttr align <> ">" <>
            cell <> "</" <> fromString constructor <> ">" <> "\n"

instance (HasPipeTable i b, Monoid b)
        => HasPipeTable (WithSourceMap i) (WithSourceMap b) where
  pipeTable aligns headerCells rows = do
    (pipeTable aligns <$> sequence headerCells <*> mapM sequence rows)
     <* addName "pipeTable"

pCells :: Monad m => ParsecT [Tok] s m [[Tok]]
pCells = try $ do
  hasPipe <- option False $ True <$ symbol '|'
  pipedCells <- many (try $ pCell <* symbol '|')
  unpipedCell <- option [] $ (:[]) <$> pCell
  let cells = pipedCells ++ unpipedCell
  guard $ not (null cells)
  guard $ hasPipe || not (null pipedCells) -- need at least one |
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
  hasPipe <- option False $ True <$ symbol '|'
  pipedAligns <- many (try $ pDivider <* symbol '|')
  unpipedAlign <- option [] $ (:[]) <$> pDivider
  let aligns = pipedAligns ++ unpipedAlign
  guard $ not (null aligns)
  guard $ hasPipe || not (null pipedAligns) -- need at least one |
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
  , syntaxFinalParsers = []
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
         notFollowedBy blankLine
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
     , blockFinalize       = \(Node ndata children) parent ->
         defaultFinalizer
           (if null (blockLines ndata)
               then Node ndata children
               else Node ndata{ blockSpec = paraSpec } children) parent
     }
