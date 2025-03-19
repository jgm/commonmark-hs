{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TypeFamilies          #-}

module Commonmark.Extensions.PipeTable
 ( HasPipeTable(..)
 , ColAlignment(..)
 , pipeTableSpec
 )
where

import Control.Monad (guard, void, mzero)
import Control.Monad.Trans.Class (lift)
import Commonmark.Syntax
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.TokParsers
import Commonmark.Blocks
import Commonmark.SourceMap
import Commonmark.Html
import Commonmark.Nodes hiding (Node (..))
import Text.Parsec
import Data.Dynamic
import Data.Tree
import Data.Data
import qualified Data.Text as T

data ColAlignment = LeftAlignedCol
                  | CenterAlignedCol
                  | RightAlignedCol
                  | DefaultAlignedCol
                  deriving (Show, Eq, Data, Typeable)

data PipeTableData = PipeTableData
     { pipeTableAlignments :: [ColAlignment]
     , pipeTableColCount   :: !Int
     , pipeTableRowCount   :: !Int
     , pipeTableCellCount  :: !Int
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
      (do symbol '\\'
          tok <- symbol '|'
          return $! [tok])
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

getAutoCompletedCellCount :: PipeTableData -> Int
getAutoCompletedCellCount tabledata =
  (numrows * numcols) - numcells
  where
    numrows = pipeTableRowCount tabledata
    numcols = pipeTableColCount tabledata
    numcells = pipeTableCellCount tabledata

-- This parser is structured as a system that parses the *second* line first,
-- then parses the first line. That is, if it detects a delimiter row as the
-- second line of a paragraph, it converts the paragraph into a table. This seems
-- counterintuitive, but it works better than trying to convert a table into
-- a paragraph, since it might need to be something else.
--
-- See GH-52 and GH-95
pipeTableBlockSpec :: (Monad m, IsBlock il bl, IsInline il,
                       HasPipeTable il bl)
                   => BlockSpec m il bl
pipeTableBlockSpec = BlockSpec
     { blockType           = "PipeTable" -- :: Text
     , blockStart          = try $ do -- :: BlockParser m il bl ()
             (cur:rest) <- nodeStack <$> getState
             guard $ blockParagraph (bspec cur)
             nonindentSpaces
             pos <- getPosition
             aligns <- pDividers
             skipWhile (hasType Spaces)
             lookAhead (eof <|> void lineEnd)

             st <- getState

             let headerLine =
                   case blockLines $ rootLabel cur of
                      [onlyLine] -> onlyLine
                      _ -> []

             cellsR <- lift $ runParserT pCells st "" headerLine
             case cellsR of
                Right cells ->
                   if length cells /= length aligns
                      then mzero -- parse fail: not a table
                      else do
                         updateState $ \st' -> st'{ nodeStack = rest }
                         let tabledata = PipeTableData
                               { pipeTableAlignments = aligns
                               , pipeTableColCount   = length cells
                               , pipeTableRowCount   = 0
                               , pipeTableCellCount  = 0
                               , pipeTableHeaders    = cells
                               , pipeTableRows       = []
                               }
                         addNodeToStack $
                            Node (defBlockData pipeTableBlockSpec){
                                    blockStartPos = blockStartPos (rootLabel cur) ++ [pos]
                                  , blockData = toDyn tabledata
                                  , blockAttributes = blockAttributes (rootLabel cur)
                                  } []
                _ ->
                   mzero -- parse fail: not a table
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
                             , pipeTableColCount = 0
                             , pipeTableRowCount = 0
                             , pipeTableCellCount = 0
                             , pipeTableHeaders = []
                             , pipeTableRows = [] }
         pos <- getPosition
         cells <- pCells
         let cells' = take (pipeTableColCount tabledata) cells
         let tabledata' =
                tabledata{ pipeTableRows = cells' : pipeTableRows tabledata
                         , pipeTableRowCount = 1 + pipeTableRowCount tabledata
                         , pipeTableCellCount = length cells' + pipeTableCellCount tabledata
                         }
         -- Protect against quadratic output size explosion.
         --
         -- Because the table extension fills in missing table cells,
         -- you can, theoretically, force the output to grows as the
         -- square of the input by adding one column and one row.
         -- This is a side-effect of the extension as specified in GFM,
         -- and follows from the geometric definition of "squaring."
         --
         -- To prevent this, we track the number of Real Cells,
         -- and if the number of autocompleted cells exceeds 200,000,
         -- stop parsing.
         guard $ getAutoCompletedCellCount tabledata <= 200000
         return $! (pos, Node ndata{ blockData =
                               toDyn tabledata' } children)
     , blockConstructor    = \(Node ndata _) -> do
         let tabledata = fromDyn
                (blockData ndata)
                PipeTableData{ pipeTableAlignments = []
                             , pipeTableColCount = 0
                             , pipeTableRowCount = 0
                             , pipeTableCellCount = 0
                             , pipeTableHeaders = []
                             , pipeTableRows = [] }
         let aligns = pipeTableAlignments tabledata
         headers <- mapM runInlineParser (pipeTableHeaders tabledata)
         let numcols = pipeTableColCount tabledata
         rows <- mapM (mapM runInlineParser . take numcols . (++ (repeat [])))
                    (reverse $ pipeTableRows tabledata)
         return $! (pipeTable aligns headers rows)
     , blockFinalize       = \(Node ndata children) parent ->
         defaultFinalizer (Node ndata children) parent
     }

data NodeTypePipeTable a
  = NodePipeTable [ColAlignment] [Nodes a] [[Nodes a]]
  deriving (Show)

instance (Typeable a, Monoid a, HasAttributes a, Rangeable a) => NodeType NodeTypePipeTable a where
  type FromNodeType NodeTypePipeTable a = HasPipeTable a a
  fromNodeType = \case
    NodePipeTable aligns headers rows -> pipeTable aligns (map fromNodes headers) (map (map fromNodes) rows)

instance ToPlainText (NodeTypePipeTable a) where
  toPlainText = \case
    NodePipeTable _ headers rows ->
      T.unlines $
        fromRow (map toPlainText headers) :
          [ fromRow (map toPlainText row)
          | row <- rows
          ]
    where
      fromRow = T.unwords

instance (Typeable a, HasPipeTable a a, Monoid a, HasAttributes a, Rangeable a) => HasPipeTable (Nodes a) (Nodes a) where
  pipeTable aligns headers rows = singleNode $ NodePipeTable aligns headers rows
