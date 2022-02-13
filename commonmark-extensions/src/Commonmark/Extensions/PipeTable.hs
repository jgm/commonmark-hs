{-# LANGUAGE IncoherentInstances   #-}
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
import Commonmark.TokParsers
import Commonmark.Blocks
import Commonmark.SourceMap
import Commonmark.Html
import Text.Parsec
import Data.Dynamic
import Data.Tree
import Data.Data

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

instance HasPipeTable (Html a) (Html a) where
  pipeTable aligns headerCells rows =
    htmlBlock "table" $ Just $ htmlRaw "\n" <>
    (if null headerCells
        then mempty
        else htmlBlock "thead" $ Just $ htmlRaw "\n" <>
             toRow "th" aligns headerCells) <>
    (if null rows
        then mempty
        else htmlBlock "tbody" $ Just $ htmlRaw "\n" <>
             mconcat (map (toRow "td" aligns) rows))
    where
      alignToAttr LeftAlignedCol    =
        addAttribute ("style","text-align: left;")
      alignToAttr CenterAlignedCol  =
        addAttribute ("style","text-align: center;")
      alignToAttr RightAlignedCol   =
        addAttribute ("style","text-align: right;")
      alignToAttr DefaultAlignedCol = id
      toRow constructor aligns' cells =
        htmlBlock "tr" $ Just $ htmlRaw "\n" <>
          mconcat (zipWith (toCell constructor) aligns' cells)
      toCell constructor align cell =
        (alignToAttr align $ htmlInline constructor $ Just cell)
          <> htmlRaw "\n"

instance (HasPipeTable i b, Monoid b)
        => HasPipeTable (WithSourceMap i) (WithSourceMap b) where
  pipeTable aligns headerCells rows = do
    (pipeTable aligns <$> sequence headerCells <*> mapM sequence rows)
     <* addName "pipeTable"

pCells :: Monad m => ParsecT [Tok] s m [[Tok]]
pCells = try $ do
  hasPipe <- option False $ True <$ symbol '|'
  pipedCells <- many (try $ pCell <* symbol '|')
  skipMany $ satisfyTok (hasType Spaces)
  unpipedCell <- option [] $ (:[]) <$> pCell
  let cells = pipedCells ++ unpipedCell
  guard $ not (null cells)
  guard $ hasPipe || not (null pipedCells) -- need at least one |
  lookAhead blankLine
  return $! cells

pCell :: Monad m => ParsecT [Tok] s m [Tok]
pCell = mconcat <$> many1
  ( try
      (do tok' <- symbol '\\'
          tok@(Tok (Symbol c) _ _) <- anySymbol
          if c == '|'
             then return $! [tok]
             else return $! [tok',tok])
  <|> (do tok <- (satisfyTok $ \t -> not (hasType (Symbol '|') t ||
                                       hasType LineEnd t))
          return $! [tok])
  ) <|> ([] <$ lookAhead (symbol '|'))

pDividers :: Monad m => ParsecT [Tok] s m [ColAlignment]
pDividers = try $ do
  hasPipe <- option False $ True <$ symbol '|'
  pipedAligns <- many (try $ pDivider <* symbol '|')
  skipMany $ satisfyTok (hasType Spaces)
  unpipedAlign <- option [] $ (:[]) <$> pDivider
  let aligns = pipedAligns ++ unpipedAlign
  guard $ not (null aligns)
  guard $ hasPipe || not (null pipedAligns) -- need at least one |
  lookAhead blankLine
  return $! aligns


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
  return $! align

-- | Syntax for pipe tables.  Note that this should generally be
-- placed AFTER the syntax spec for lists, headings, and other block-level
-- constructs, to avoid bad results when non-table lines contain pipe
-- characters:  use @defaultSyntaxSpec <> pipeTableSpec@ rather
-- than @pipeTableSpec <> defaultSyntaxSpec@.
pipeTableSpec :: (Monad m, IsBlock il bl, IsInline il, HasPipeTable il bl)
              => SyntaxSpec m il bl
pipeTableSpec = mempty
  { syntaxBlockSpecs = [pipeTableBlockSpec]
  }

pipeTableBlockSpec :: (Monad m, IsBlock il bl, IsInline il,
                       HasPipeTable il bl)
                   => BlockSpec m il bl
pipeTableBlockSpec = BlockSpec
     { blockType           = "PipeTable" -- :: Text
     , blockStart          = try $ do -- :: BlockParser m il bl ()
         interruptsParagraph >>= guard . not
         nonindentSpaces
         notFollowedBy whitespace
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
         return BlockStartMatch
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
             return $! (pos, Node ndata{ blockData =
                                   toDyn tabledata' } children)
           else
             -- last line was first; check for separators
             -- and if not found, convert to paragraph:
             try (do aligns <- pDividers
                     guard $ length aligns ==
                             length (pipeTableHeaders tabledata)
                     let tabledata' = tabledata{ pipeTableAlignments = aligns }
                     return $! (pos, Node ndata{
                                              blockLines = []
                                            , blockData = toDyn tabledata'
                                            } children))
             <|> (return $! (pos, Node ndata{
                                   blockSpec = paraSpec } children))
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
         return $! (pipeTable aligns headers rows)
     , blockFinalize       = \(Node ndata children) parent ->
         defaultFinalizer
           (if null (blockLines ndata)
               then Node ndata children
               else Node ndata{ blockSpec = paraSpec } children) parent
     }
