{-# LANGUAGE CPP                   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE StrictData            #-}
module Commonmark.Blocks
  ( mkBlockParser
  , defaultBlockSpecs
  , BlockStartResult(..)
  , BlockSpec(..)
  , BlockData(..)
  , defBlockData
  , BlockNode
  , BPState(..)
  , BlockParser
  , LinkInfo(..)
  , defaultFinalizer
  , runInlineParser
  , addRange
  , addNodeToStack
  , collapseNodeStack
  , getBlockText
  , removeIndent
  , bspec
  , endOfBlock
  , interruptsParagraph
  , linkReferenceDef
  , renderChildren
  -- * BlockSpecs
  , docSpec
  , indentedCodeSpec
  , fencedCodeSpec
  , blockQuoteSpec
  , atxHeadingSpec
  , setextHeadingSpec
  , thematicBreakSpec
  , listItemSpec
  , bulletListMarker
  , orderedListMarker
  , rawHtmlSpec
  , attributeSpec
  , paraSpec
  )
where

import           Commonmark.Tag
import           Commonmark.Parsec
import           Commonmark.ReferenceMap
import           Commonmark.Inlines        (pEscaped, pLinkDestination,
                                            pLinkLabel, pLinkTitle)
import           Commonmark.Entity         (unEntity)
import           Commonmark.Types
import           Control.Monad             (foldM, guard, mzero, void, unless,
                                            when)
import           Control.Monad.Trans.Class (lift)
import           Data.Foldable             (foldrM)
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid
#endif
import           Data.Char                 (isAsciiUpper, isDigit, isSpace,
                                            isLetter, isAlphaNum)
import           Data.Dynamic
import           Data.List                 (sort)
import           Data.Text                 (Text)
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import qualified Data.Text.Read            as TR
import           Data.Tree
import           Data.Void                 (Void)
import Debug.Trace

mkBlockParser
  :: (Monad m, IsBlock il bl)
  => [BlockSpec m il bl] -- ^ Defines block syntax
  -> [BlockParser m il bl bl] -- ^ Parsers to run at end
  -> (ReferenceMap ->
      [((SourcePos, SourcePos), Text)] ->
      m (Either (ParseErrorBundle Text Void) il)) -- ^ Inline parser
  -> [BlockParser m il bl Attributes] -- ^ attribute parsers
  -> String -- ^ Name of source
  -> Text -- ^ Tokenized commonmark input
  -> m (Either (ParseErrorBundle Text Void) bl)  -- ^ Result or error
mkBlockParser specs finalParsers ilParser attrParsers sourcename t =
  runParserT (processLines specs finalParsers)
          BPState{ referenceMap     = emptyReferenceMap
                 , inlineParser     = ilParser
                 , nodeStack        = [Node (defBlockData docSpec) []]
                 , blockMatched     = False
                 , maybeLazy        = False
                 , maybeBlank       = True
                 , counters         = M.empty
                 , failurePositions = M.empty
                 , attributeParsers = attrParsers
                 , nextAttributes   = mempty
                 } sourcename t

processLines :: (Monad m, IsBlock il bl)
             => [BlockSpec m il bl]
             -> [BlockParser m il bl bl] -- ^ Parsers to run at end
             -> BlockParser m il bl bl
processLines specs finalParsers = do
  skipMany (processLine specs)
  eof
  tree <- (nodeStack <$> getState) >>= collapseNodeStack
  updateState $ \st -> st{ nodeStack = [reverseSubforests tree] }
  endContent <- mconcat <$> sequence finalParsers
  tree':_ <- nodeStack <$> getState
  body <- blockConstructor (blockSpec (rootLabel tree')) tree'
  return $! body <> endContent
{-# SCC processLines #-}

reverseSubforests :: Tree a -> Tree a
reverseSubforests (Node x cs) = Node x $ map reverseSubforests $ reverse cs

processLine :: (Monad m, IsBlock il bl)
            => [BlockSpec m il bl] -> BlockParser m il bl ()
processLine specs = do
  -- check block continuations for each node in stack
  st' <- getState
  putState $  st'{ blockMatched = True
                 , maybeLazy = False
                 , maybeBlank = True
                 , failurePositions = M.empty }
  (matched, unmatched) <-  foldrM checkContinue ([],[]) (nodeStack st')

  -- if not everything matched, and last unmatched is paragraph,
  -- then we may have a lazy paragraph continuation
  updateState $ \st -> st{ maybeLazy =
    case unmatched of
         m:_ -> blockParagraph (bspec m)
         _   -> False }

  -- close unmatched blocks
  if null unmatched
    then updateState $ \st -> st{ nodeStack = matched }
         -- this update is needed or we lose startpos information
    else case matched of
              []   -> error "no blocks matched"
              m:ms -> do
                m' <- collapseNodeStack (unmatched ++ [m])
                updateState $ \st -> st{ nodeStack = m':ms }

  isblank <- option False $ True <$ (do getState >>= guard . maybeBlank
                                        lookAhead blankLine)
  {-# SCC block_starts #-} unless isblank $
    (do skipSome (doBlockStarts specs)
        optional (try (blockStart paraSpec))
        return ())
      <|>
    (do getState >>= guard . maybeLazy
        -- lazy line
        sp <- getPosition
        updateState $ \st -> st{ nodeStack =
             map (addStartPos sp) (unmatched ++ matched) })
      <|>
    void (try (blockStart paraSpec))
      <|>
    return ()

  (cur:rest) <- nodeStack <$> getState
  -- add line contents
  !startTextPos <- getPosition
  !(toks, endpos) <- {-# SCC restOfLine #-} restOfLine
  let curdata = rootLabel cur
  updateState $ \st -> st{
      nodeStack = map (addEndPos endpos) $
        cur{ rootLabel =
               if blockContainsLines (bspec cur)
                  then curdata{ blockLines = ((startTextPos, endpos), toks)
                                               : blockLines curdata }
                  else
                    if isblank
                       then curdata{ blockBlanks = unPos (sourceLine endpos)
                                       : blockBlanks curdata }
                       else curdata
           } : rest
      }
  -- showNodeStack

addStartPos :: SourcePos -> BlockNode m il bl -> BlockNode m il bl
addStartPos sp (Node bd cs) = Node bd{ blockStartPos = sp : blockStartPos bd } cs

addEndPos :: SourcePos -> BlockNode m il bl -> BlockNode m il bl
addEndPos endpos (Node bdata children) =
  Node bdata{ blockEndPos = endpos : blockEndPos bdata } children

doBlockStarts :: Monad m => [BlockSpec m il bl] -> BlockParser m il bl ()
doBlockStarts specs = do
  st' <- getState
  initPos <- getPosition
  let failurePosMap = failurePositions st'
  let specs' = foldr (\spec sps ->
                        case M.lookup (blockType spec) failurePosMap of
                          Just pos' | initPos < pos' -> sps
                          _ -> spec:sps) [] specs
  go initPos specs'
 where
  go _ [] = mzero
  go initPos (spec:otherSpecs) = try (do
    pst <- getParserState
    res <- blockStart spec
    case res of
      BlockStartMatch -> return ()
      BlockStartNoMatchBefore pos -> do
        setParserState pst
        unless (pos == initPos) $
          updateState $ \st ->
             st{ failurePositions =
                  M.insert (blockType spec)
                  pos (failurePositions st) }
        go initPos otherSpecs) <|> go initPos otherSpecs
  {-# SCC go #-}

checkContinue :: Monad m
              => BlockNode m il bl
              -> ([BlockNode m il bl],[BlockNode m il bl])
              -> BlockParser m il bl ([BlockNode m il bl],[BlockNode m il bl])
checkContinue nd (matched, unmatched) = do
  ismatched <- blockMatched <$> getState
  if ismatched
     then
       {-# SCC blockContinues #-}
       (do (startpos, Node bdata children) <- blockContinue (bspec nd) nd
           matched' <- blockMatched <$> getState
           -- if blockContinue set blockMatched to False, it's
           -- because of characters on the line closing the block,
           -- so it's not to be counted as blank:
           unless matched' $
             updateState $ \st -> st{ maybeBlank = False }
           pos' <- if matched'
                      then return $! startpos
                      else getPosition
           let new = Node bdata{ blockStartPos =
                      startpos : blockStartPos bdata,
                      blockEndPos =
                         if matched'
                            then blockEndPos bdata
                            else pos' : blockEndPos bdata
                      } children
           return $!
             if matched'
                then (new:matched, unmatched)
                else (matched, new:unmatched))
       <|> (matched, nd:unmatched) <$ updateState (\st -> st{
                                         blockMatched = False })
     else return $! (matched, nd:unmatched)


{-
--- for debugging
showNodeStack :: Monad m => BlockParser m il bl a
showNodeStack = do
  ns <- nodeStack <$> getState
  trace (unlines ("NODESTACK:" : map showNode ns)) (return $! ())
  return undefined
 where
 showNode (Node bdata children) =
   unlines [ "-----"
           , show (blockSpec bdata)
           , show (blockStartPos bdata)
           , show (blockEndPos bdata)
           , show (length  children) ]
-}

data BlockStartResult =
    BlockStartMatch
  | BlockStartNoMatchBefore !SourcePos
  deriving (Show, Eq)

-- | Defines a block-level element type.
data BlockSpec m il bl = BlockSpec
     { blockType           :: !Text  -- ^ Descriptive name of block type
     , blockStart          :: BlockParser m il bl BlockStartResult
                           -- ^ Parses beginning
                           -- of block.  The parser should verify any
                           -- preconditions, parse the opening of the block,
                           -- and add the new block to the block stack using
                           -- 'addNodeToStack', returning 'BlockStartMatch' on
                           -- success. If the match fails, the parser can
                           -- either fail or return 'BlockStartNoMatchBefore' and a
                           -- 'SourcePos' before which the parser is known
                           -- not to succeed (this will be stored in
                           -- 'failurePositions' for the line, to ensure
                           -- that future matches won't be attempted until
                           -- after that position).
     , blockCanContain     :: BlockSpec m il bl -> Bool -- ^ Returns True if
                           -- this kind of block can contain the specified
                           -- block type.
     , blockContainsLines  :: !Bool -- ^ True if this kind of block
                           -- can contain text lines.
     , blockParagraph      :: !Bool -- ^ True if this kind of block
                           -- is paragraph.
     , blockContinue       :: BlockNode m il bl
                           -> BlockParser m il bl (SourcePos, BlockNode m il bl)
                           -- ^ Parser that checks to see if the current
                           -- block (the 'BlockNode') can be kept open.
                           -- If it fails, the block will be closed, unless
                           -- we have a lazy paragraph continuation within
                           -- the block.
     , blockConstructor    :: BlockNode m il bl -> BlockParser m il bl bl
                           -- ^ Renders the node into its target format,
                           -- possibly after rendering inline content.
     , blockFinalize       :: BlockNode m il bl -> BlockNode m il bl
                           -> BlockParser m il bl (BlockNode m il bl)
                           -- ^ Runs when the block is closed, but prior
                           -- to rendering.  The first parameter is the
                           -- child, the second the parent.
     }

instance Show (BlockSpec m il bl) where
  show bs = "<BlockSpec " ++ T.unpack (blockType bs) ++ ">"

defaultBlockSpecs :: (Monad m, IsBlock il bl) => [BlockSpec m il bl]
defaultBlockSpecs =
    [ indentedCodeSpec
    , fencedCodeSpec
    , blockQuoteSpec
    , atxHeadingSpec
    , setextHeadingSpec
    , thematicBreakSpec
    , listItemSpec (bulletListMarker <|> orderedListMarker)
    , rawHtmlSpec
    , attributeSpec
    ]

defaultFinalizer :: Monad m
                 => BlockNode m il bl
                 -> BlockNode m il bl
                 -> BlockParser m il bl (BlockNode m il bl)
defaultFinalizer !child !parent = do
  -- ensure that 'counters' carries information about all
  -- the block identifiers used, so that auto_identifiers works properly.
  case lookup "id" (blockAttributes (rootLabel child)) of
    Nothing -> return ()
    Just !ident -> updateState $ \st ->
      st{ counters = M.insert ("identifier:" <> ident)
          (toDyn (0 :: Int)) (counters st) }
  return $! parent{ subForest = child : subForest parent }

data BlockData m il bl = BlockData
     { blockSpec       :: BlockSpec m il bl
     , blockLines      :: [((SourcePos, SourcePos), Text)]
                                       -- in reverse order
     , blockStartPos   :: [SourcePos]  -- in reverse order
     , blockEndPos     :: [SourcePos]  -- reverse order
     , blockData       :: !Dynamic
     , blockBlanks     :: [Int]  -- non-content blank lines in block
     , blockAttributes :: !Attributes
     }
  deriving Show

defBlockData :: BlockSpec m il bl -> BlockData m il bl
defBlockData spec = BlockData
    { blockSpec     = spec
    , blockLines    = []
    , blockStartPos = []
    , blockEndPos   = []
    , blockData     = toDyn ()
    , blockBlanks   = []
    , blockAttributes = mempty
    }

type BlockNode m il bl = Tree (BlockData m il bl)

data BPState m il bl = BPState
     { referenceMap     :: !ReferenceMap
     , inlineParser     :: ReferenceMap ->
                           [((SourcePos, SourcePos), Text)] ->
                           m (Either (ParseErrorBundle Text Void) il)
     , nodeStack        :: [BlockNode m il bl]   -- reverse order, head is tip
     , blockMatched     :: !Bool
     , maybeLazy        :: !Bool
     , maybeBlank       :: !Bool
     , counters         :: M.Map Text Dynamic
     , failurePositions :: M.Map Text SourcePos  -- record known positions
                           -- where parsers fail to avoid repetition
     , attributeParsers :: [ParsecT Text (BPState m il bl) m Attributes]
     , nextAttributes   :: !Attributes
     }

type BlockParser m il bl = ParsecT Text (BPState m il bl) m

data ListData = ListData
     { listType    :: !ListType
     , listSpacing :: !ListSpacing
     } deriving (Show, Eq)

data ListItemData = ListItemData
     { listItemType         :: !ListType
     , listItemIndent       :: !Int
     , listItemBlanksInside :: !Bool
     , listItemBlanksAtEnd  :: !Bool
     } deriving (Show, Eq)

runInlineParser :: (Monad m, Monoid il)
                => [((SourcePos, SourcePos), Text)]
                -> BlockParser m il bl il
runInlineParser tls = do
  refmap <- referenceMap <$> getState
  ilParser <- inlineParser <$> getState
  res <- lift . lift $ ilParser refmap tls
  case res of
       Right ils -> return $! ils
       Left err  -> mapM registerError (bundleErrors err) >> return mempty
         where
          registerError (FancyError _i fncyset) = fancyFailure fncyset
          registerError (TrivialError _i mbee setee) = failure mbee setee

{-# SCC runInlineParser #-}

addRange :: (Monad m, IsBlock il bl)
         => BlockNode m il bl -> bl -> bl
addRange (Node b _)
 = ranged (SourceRange
            (go . reverse $ zip (blockStartPos b) (blockEndPos b)))
   where
     go [] = []
     go ((!startpos1, _):(!startpos2, !endpos2):rest)
       | sourceColumn startpos2 == mkPos 1 = go ((startpos1, endpos2):rest)
     go (!x:xs) = x : go xs

-- Add a new node to the block stack.  If current tip can contain
-- it, add it there; otherwise, close the tip and repeat til we get
-- to a block that can contain this node.
addNodeToStack :: Monad m => BlockNode m bl il -> BlockParser m bl il ()
addNodeToStack node = do
  (cur:rest) <- nodeStack <$> getState
  guard $ blockParagraph (bspec cur) || not (blockContainsLines (bspec cur))
  if blockCanContain (bspec cur) (bspec node)
     then do
       nextAttr <- nextAttributes <$> getState
       let node' = if null nextAttr
                      then node
                      else
                        let rl = rootLabel node
                        in  node{ rootLabel = rl{
                                  blockAttributes = nextAttr
                                }}
       updateState $ \st ->
            st{ nextAttributes = mempty
              , nodeStack = node' : cur : rest
              , maybeLazy = False }
     else case rest of
              (x:xs) -> do
                stack <- (:xs) <$> collapseNodeStack [cur,x]
                updateState $ \st -> st{ nodeStack = stack }
                addNodeToStack node
              _ -> mzero

isSpaceChar :: Char -> Bool
isSpaceChar ' '  = True
isSpaceChar '\t' = True
isSpaceChar _    = False

isLineEndChar :: Char -> Bool
isLineEndChar '\n'  = True
isLineEndChar '\r'  = True
isLineEndChar _     = False

interruptsParagraph :: Monad m => BlockParser m bl il Bool
interruptsParagraph = do
  (cur:_) <- nodeStack <$> getState
  return $! blockParagraph (bspec cur)

renderChildren :: (Monad m, IsBlock il bl)
               => BlockNode m il bl -> BlockParser m il bl [bl]
renderChildren node = mapM renderC $ subForest node
  where
    renderC n = do
      let attrs = blockAttributes (rootLabel n)
      (if null attrs
          then id
          else addAttributes attrs) <$>
        blockConstructor (blockSpec (rootLabel n)) n

docSpec :: (Monad m, IsBlock il bl, Monoid bl) => BlockSpec m il bl
docSpec = BlockSpec
     { blockType           = "Doc"
     , blockStart          = mzero
     , blockCanContain     = const True
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \n -> (,n) <$> getPosition
     , blockConstructor    = fmap mconcat . renderChildren
     , blockFinalize       = defaultFinalizer
     }

refLinkDefSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
refLinkDefSpec = BlockSpec
     { blockType           = "ReferenceLinkDefinition"
     , blockStart          = mzero
     , blockCanContain     = const False
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = const mzero
     , blockConstructor    = \node -> do
         let linkdefs = fromDyn (blockData (rootLabel node))
                  [] :: [((SourceRange, Text), (Text, Text))]
         return $! mconcat $ map (\((range, lab), (dest, tit)) ->
            (ranged range
              (referenceLinkDefinition lab (dest, tit)))) linkdefs
     , blockFinalize       = defaultFinalizer
     }

-- Parse reference links from beginning of block text;
-- update reference map and block text; return maybe altered node
-- (if it still contains lines) and maybe ref link node.
extractReferenceLinks :: (Monad m, IsBlock il bl)
                      => BlockNode m il bl
                      -> BlockParser m il bl (Maybe (BlockNode m il bl),
                                              Maybe (BlockNode m il bl))
extractReferenceLinks node = do
  st <- getState
  let startPos = case reverse (blockStartPos (rootLabel node)) of
                   (x:_) -> x
                   _     -> initialPos ""
  res <- lift . lift $ runParserT
                 (do setPosition startPos
                     ds <- some (linkReferenceDef (choice $ attributeParsers st))
                     pos <- getPosition
                     return (ds, pos))
                 st "" (getBlockText removeIndent node)
  case res of
        Left _ -> return $! (Just node, Nothing)
        Right (linkdefs, pos) -> do
          mapM_
            (\((_,lab),linkinfo) ->
             updateState $ \s -> s{
              referenceMap = insertReference lab linkinfo
                (referenceMap s) }) linkdefs
          let toks' = takeWhile (\((sp,_),_) -> sp >= pos)
                         (blockLines (rootLabel node))
          let node' = if null toks'
                         then Nothing
                         else Just node{ rootLabel =
                              (rootLabel node){
                                blockLines = toks',
                                blockStartPos = takeWhile (>= pos)
                                   (blockStartPos (rootLabel node)),
                                blockEndPos = takeWhile (>= pos)
                                   (blockEndPos (rootLabel node))
                                }
                           }
          let refnode = node{ rootLabel =
                 (rootLabel node){
                     blockLines = dropWhile (\((sp,_),_) -> sp >= pos)
                       (blockLines (rootLabel node))
                   , blockStartPos = dropWhile (>= pos)
                        (blockStartPos (rootLabel node))
                   , blockEndPos = dropWhile (>= pos)
                        (blockEndPos (rootLabel node))
                   , blockData = toDyn linkdefs
                   , blockSpec = refLinkDefSpec
                 }}
          return $! (node', Just refnode)

attributeSpec :: (Monad m, IsBlock il bl)
              => BlockSpec m il bl
attributeSpec = BlockSpec
     { blockType           = "Attribute"
     , blockStart          = do
         attrParsers <- attributeParsers <$> getState
         guard $ not (null attrParsers)
         interruptsParagraph >>= guard . not
         nonindentSpaces
         pos <- getPosition
         attrs <- choice attrParsers
         skipWhile isSpaceChar
         lookAhead (void lineEnd <|> eof)
         addNodeToStack $
           Node (defBlockData attributeSpec){
                     blockData = toDyn attrs,
                     blockStartPos = [pos] } []
         return BlockStartMatch
     , blockCanContain     = const False
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \n -> do
         attrParsers <- attributeParsers <$> getState
         guard $ not (null attrParsers)
         nonindentSpaces
         pos <- getPosition
         attrs <- choice attrParsers
         skipWhile isSpaceChar
         lookAhead (void lineEnd <|> eof)
         let oldattrs = fromDyn (blockData (rootLabel n)) mempty :: Attributes
         let attrs' = oldattrs <> attrs
         return $! (pos, n{ rootLabel = (rootLabel n){
                          blockData = toDyn attrs' }})
     , blockConstructor    = \_ -> return $! mempty
     , blockFinalize       = \node parent -> do
         let attrs = fromDyn (blockData (rootLabel node)) mempty :: Attributes
         updateState $ \st -> st{ nextAttributes = attrs }
         defaultFinalizer node parent
     }

paraSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
paraSpec = BlockSpec
     { blockType           = "Paragraph"
     , blockStart          = do
             interruptsParagraph >>= guard . not
             skipWhile isSpaceChar
             pos <- getPosition
             notFollowedBy lineEnd
             addNodeToStack $
               Node (defBlockData paraSpec){
                       blockStartPos = [pos] } []
             return BlockStartMatch
     , blockCanContain     = const False
     , blockContainsLines  = True
     , blockParagraph      = True
     , blockContinue       = \n -> lookAhead $ try $ do
             skipWhile isSpaceChar
             pos <- getPosition
             notFollowedBy lineEnd
             return $! (pos, n)
     , blockConstructor    = \node ->
         (addRange node . paragraph)
             <$> runInlineParser (reverse . blockLines . rootLabel $ node)
     , blockFinalize       = \child parent -> do
         (mbchild, mbrefdefs) <- extractReferenceLinks child
         case (mbchild, mbrefdefs) of
           (_, Nothing) -> defaultFinalizer child parent
           (Nothing, Just refnode)
                        -> return $! parent{ subForest =
                                          refnode : subForest parent }
           (Just child', Just refnode)
                        -> return $! parent{ subForest =
                                        child' : refnode : subForest parent }
     }

plainSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
plainSpec = paraSpec{
    blockConstructor    = \node ->
         (addRange node . plain)
             <$> runInlineParser (reverse . blockLines . rootLabel $ node)
  }


linkReferenceDef :: Monad m
                 => ParsecT Text s m Attributes
                 -> ParsecT Text s m ((SourceRange, Text), LinkInfo)
linkReferenceDef attrParser = try $ do
  startpos <- getPosition
  lab <- pLinkLabel
  guard $ not $ T.all isSpace lab
  char ':'
  optional whitespace
  dest <- pLinkDestination
  (title, attrs) <- option (mempty, mempty) $ try $ do
             tit <- option mempty $ try (whitespace *> pLinkTitle)
             skipWhile isSpaceChar
             as <- option mempty attrParser
             skipWhile isSpaceChar
             lookAhead (void lineEnd <|> eof)
             return $! (tit, as)
  endpos <- getPosition
  void lineEnd <|> eof
  return $! ((SourceRange [(startpos, endpos)], lab),
                LinkInfo{ linkDestination = unEntity dest
                        , linkTitle = unEntity title
                        , linkAttributes = attrs })

atxHeadingSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
atxHeadingSpec = BlockSpec
     { blockType           = "ATXHeading"
     , blockStart          = do
             nonindentSpaces
             pos <- getPosition
             hashes <- some (char '#')
             let level = length hashes
             guard $ level <= 6
             void (satisfy isSpaceChar)
                <|> void (lookAhead lineEnd)
                <|> lookAhead eof
             raw <- option mempty $ textWhile1 (\c -> c /= '\r' && c /= '\n')
             endpos <- getPosition
             -- trim off closing ###
             let rawtrimmed = T.stripEnd raw
             let raw' = case T.unsnoc $ T.dropWhileEnd (=='#') rawtrimmed of
                          Just (t,c)
                            | c == ' ' || c == '\t' -> t
                            | otherwise             -> rawtrimmed
                          Nothing -> mempty
             addNodeToStack $ Node (defBlockData atxHeadingSpec){
                            blockLines = [((pos, endpos), raw')],
                            blockData = toDyn level,
                            blockStartPos = [pos] } []
             return BlockStartMatch
     , blockCanContain     = const False
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = const mzero
     , blockConstructor    = \node -> do
         let level = fromDyn (blockData (rootLabel node)) 1
         ils <- runInlineParser (reverse . blockLines . rootLabel $ node)
         return $! (addRange node . heading level) ils
     , blockFinalize       = \node@(Node cdata children) parent -> do
         let oldAttr = blockAttributes cdata
         let toks = getBlockText removeIndent node
         (newtoks, attr) <- parseFinalAttributes True toks
                        <|> (return $! (toks, mempty))
         let newlns = case blockLines cdata of
                        (((sp,ep),_):zs) -> ((sp,ep),newtoks):zs
                        []               -> []
         defaultFinalizer (Node cdata{ blockAttributes = oldAttr <> attr
                                     , blockLines = newlns }
                                children) parent
     }

setextHeadingSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
setextHeadingSpec = BlockSpec
     { blockType           = "SetextHeading"
     , blockStart          = do
             (cur:rest) <- nodeStack <$> getState
             guard $ blockParagraph (bspec cur)
             nonindentSpaces
             pos <- getPosition
             level <- (2 :: Int) <$ skipSome (char '-')
                  <|> (1 :: Int) <$ skipSome (char '=')
             skipWhile isSpaceChar
             lookAhead (eof <|> void lineEnd)
             -- process any reference links, make sure there's some
             -- content left
             (mbcur, mbrefdefs) <- extractReferenceLinks cur
             updateState $ \st ->
                st{ nodeStack = case mbrefdefs of
                                  Nothing -> rest
                                  Just rd -> case rest of
                                                (x:xs) ->
                                                  x{ subForest =
                                                      rd : subForest x }:xs
                                                [] -> [rd] }
             case mbcur of
               Nothing -> mzero -- should not happen
               Just cur' -> do
                 -- replace cur with new setext heading node
                 addNodeToStack $
                      Node (rootLabel cur'){
                              blockSpec  = setextHeadingSpec,
                              blockData = toDyn level,
                              blockStartPos =
                                   pos : blockStartPos (rootLabel cur') }
                                    []
                 return BlockStartMatch
     , blockCanContain     = const False
     , blockContainsLines  = True
     , blockParagraph      = False
     , blockContinue       = const mzero
     , blockConstructor    = \node -> do
         let level = fromDyn (blockData (rootLabel node)) 1
         ils <- runInlineParser (reverse . blockLines . rootLabel $ node)
         return $! (addRange node . heading level) ils
     , blockFinalize       = \node@(Node cdata children) parent -> do
         let oldAttr = blockAttributes cdata
         case blockLines cdata of
           [] -> defaultFinalizer node parent
           (p, t):rest -> do
             (t', attr) <- parseFinalAttributes True t
                            <|> (return $! (t, mempty))
             defaultFinalizer (Node cdata{ blockAttributes = oldAttr <> attr
                                         , blockLines = (p,t'):rest }
                                    children) parent
     }

parseFinalAttributes :: Monad m
                     => Bool -> Text -> BlockParser m il bl (Text, Attributes)
parseFinalAttributes requireWhitespace ts = do
  attrParsers <- attributeParsers <$> getState
  let pAttr' = try $ (if requireWhitespace
                         then () <$ whitespace
                         else () <$ optional whitespace)
                     *> choice attrParsers <* optional whitespace <* eof
  st <- getState
  res <- lift . lift $ runParserT
       ((,) <$> (T.pack <$> many (notFollowedBy pAttr' >> anyChar))
            <*> option [] pAttr') st "heading contents" ts
  case res of
    Left _         -> mzero
    Right (xs, ys) -> return $! (xs, ys)

blockQuoteSpec :: (Monad m, IsBlock il bl) => BlockSpec m il bl
blockQuoteSpec = BlockSpec
     { blockType           = "BlockQuote"
     , blockStart          = do
             nonindentSpaces
             pos <- getPosition
             _ <- char '>'
             _ <- option 0 (gobbleSpaces 1)
             addNodeToStack $
                Node (defBlockData blockQuoteSpec){
                          blockStartPos = [pos] } []
             return BlockStartMatch
     , blockCanContain     = const True
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \n -> try $ do
             nonindentSpaces
             pos <- getPosition
             _ <- char '>'
             _ <- gobbleUpToSpaces 1
             return $! (pos, n)
     , blockConstructor    = \node ->
          (addRange node . blockQuote . mconcat) <$> renderChildren node
     , blockFinalize       = defaultFinalizer
     }

listItemSpec :: (Monad m, IsBlock il bl)
             => BlockParser m il bl ListType
             -> BlockSpec m il bl
listItemSpec parseListMarker = BlockSpec
     { blockType           = "ListItem"
     , blockStart          = do
             (pos, lidata) <- itemStart parseListMarker
             let linode = Node (defBlockData $ listItemSpec parseListMarker){
                             blockData = toDyn lidata,
                             blockStartPos = [pos] } []
             let listdata = ListData{
                    listType = listItemType lidata
                  , listSpacing = TightList }
                  -- spacing gets set in finalize
             let listnode = Node (defBlockData listSpec){
                              blockData = toDyn listdata,
                              blockStartPos = [pos] } []
             -- list can only interrupt paragraph if bullet
             -- list or ordered list w/ startnum == 1,
             -- and not followed by blank
             (cur:_) <- nodeStack <$> getState
             when (blockParagraph (bspec cur)) $ do
               guard $ case listType listdata of
                            BulletList _            -> True
                            OrderedList 1 Decimal _ -> True
                            _                       -> False
               notFollowedBy blankLine
             let curdata = fromDyn (blockData (rootLabel cur))
                                (ListData undefined undefined)
             let matchesList (BulletList c) (BulletList d)       = c == d
                 matchesList (OrderedList _ e1 d1)
                             (OrderedList _ e2 d2) = e1 == e2 && d1 == d2
                 matchesList _ _                                 = False
             case blockType (bspec cur) of
                  "List" | listType curdata `matchesList`
                           listItemType lidata
                    -> addNodeToStack linode
                  _ -> addNodeToStack listnode >> addNodeToStack linode
             return BlockStartMatch
     , blockCanContain     = const True
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \node@(Node ndata children) -> do
             let lidata = fromDyn (blockData ndata)
                             (ListItemData undefined undefined
                                           undefined undefined)
             -- a marker followed by two blanks is just an empty item:
             guard $ null (blockBlanks ndata) ||
                     not (null children)
             gobbleSpaces (listItemIndent lidata) <|> 0 <$ lookAhead blankLine
             pos <- getPosition
             return $! (pos, node)
     , blockConstructor    = fmap mconcat . renderChildren
     , blockFinalize       = \(Node cdata children) parent -> do
          let lidata = fromDyn (blockData cdata)
                                 (ListItemData undefined undefined
                                               undefined undefined)
          let blanks = removeConsecutive $ sort $
                         concat $ blockBlanks cdata :
                                  map (blockBlanks . rootLabel)
                                  (filter ((== "List") . blockType .
                                   blockSpec . rootLabel) children)
          curline <- sourceLine <$> getPosition
          let blanksAtEnd = case blanks of
                                   (l:_) -> l >= unPos curline - 1
                                   _     -> False
          let blanksInside = case length blanks of
                                n | n > 1     -> True
                                  | n == 1    -> not blanksAtEnd
                                  | otherwise -> False
          let lidata' = toDyn $ lidata{ listItemBlanksInside = blanksInside
                                      , listItemBlanksAtEnd  = blanksAtEnd }
          defaultFinalizer (Node cdata{ blockData = lidata' } children)
                           parent
     }

itemStart :: Monad m
          => BlockParser m il bl ListType
          -> BlockParser m il bl (SourcePos, ListItemData)
itemStart parseListMarker = do
  beforecol <- sourceColumn <$> getPosition
  gobbleUpToSpaces 3
  pos <- getPosition
  ty <- parseListMarker
  aftercol <- sourceColumn <$> getPosition
  lookAhead whitespace
  numspaces <- try (gobbleUpToSpaces 4 <* notFollowedBy whitespace)
           <|> gobbleSpaces 1
           <|> 1 <$ lookAhead lineEnd
  return $! (pos, ListItemData{
           listItemType = ty
          , listItemIndent = (unPos aftercol - unPos beforecol) + numspaces
          , listItemBlanksInside = False
          , listItemBlanksAtEnd = False
          })

bulletListMarker :: Monad m => BlockParser m il bl ListType
bulletListMarker = do
  c <- char '-' <|> char '*' <|> char '+'
  return $! BulletList c

orderedListMarker :: Monad m => BlockParser m il bl ListType
orderedListMarker = try $ do
  ds <- textWhile1 isDigit
  guard $ T.length ds < 10
  (start :: Int) <- either fail (return . fst) (TR.decimal ds)
  delimtype <- Period <$ char '.' <|> OneParen <$ char ')'
  return $! OrderedList start Decimal delimtype

listSpec :: (Monad m, IsBlock il bl) => BlockSpec m il bl
listSpec = BlockSpec
     { blockType           = "List"
     , blockStart          = mzero
     , blockCanContain     = \sp -> blockType sp == "ListItem"
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \n -> (,n) <$> getPosition
     , blockConstructor    = \node -> do
          let ListData lt ls = fromDyn (blockData (rootLabel node))
                                 (ListData undefined undefined)
          (addRange node . list lt ls) <$> renderChildren node
     , blockFinalize       = \(Node cdata children) parent -> do
          let ListData lt _ = fromDyn (blockData cdata)
                                 (ListData undefined undefined)
          let getListItemData (Node d _) =
                fromDyn (blockData d)
                  (ListItemData undefined undefined undefined undefined)
          let childrenData = map getListItemData children
          let ls = case childrenData of
                          c:cs | any listItemBlanksInside (c:cs) ||
                                 (not (null cs) &&
                                  any listItemBlanksAtEnd cs)
                               -> LooseList
                          _    -> TightList
          blockBlanks' <- case childrenData of
                             c:_ | listItemBlanksAtEnd c -> do
                                 curline <- sourceLine <$> getPosition
                                 return $! unPos curline - 1 :
                                           blockBlanks cdata
                             _ -> return $! blockBlanks cdata
          let ldata' = toDyn (ListData lt ls)
          -- need to transform paragraphs on tight lists
          let totight (Node nd cs)
                | blockType (blockSpec nd) == "Paragraph"
                            = Node nd{ blockSpec = plainSpec } cs
                | otherwise = Node nd cs
          let childrenToTight (Node nd cs) = Node nd (map totight cs)
          let children' =
                 if ls == TightList
                    then map childrenToTight children
                    else children
          defaultFinalizer (Node cdata{ blockData = ldata'
                                      , blockBlanks = blockBlanks' } children')
                           parent
     }

thematicBreakSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
thematicBreakSpec = BlockSpec
     { blockType           = "ThematicBreak"
     , blockStart          = do
            nonindentSpaces
            pos <- getPosition
            c <- char '-' <|> char '_' <|> char '*'
            skipWhile isSpaceChar
            let tbchar = char c <* skipWhile isSpaceChar
            count 2 tbchar
            skipMany tbchar
            (do lookAhead lineEnd
                addNodeToStack (Node (defBlockData thematicBreakSpec){
                                   blockStartPos = [pos] } [])
                return BlockStartMatch) <|>
              (BlockStartNoMatchBefore <$> getPosition)
     , blockCanContain     = const False
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = const mzero
     , blockConstructor    = \node ->
             return $! (addRange node thematicBreak)
     , blockFinalize       = defaultFinalizer
     }

indentedCodeSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
indentedCodeSpec = BlockSpec
     { blockType           = "IndentedCode"
     , blockStart          = do
             interruptsParagraph >>= guard . not
             getState >>= guard . not . maybeLazy
             _ <- gobbleSpaces 4
             pos <- getPosition
             notFollowedBy blankLine
             addNodeToStack $ Node (defBlockData indentedCodeSpec){
                          blockStartPos = [pos] } []
             return BlockStartMatch
     , blockCanContain     = const False
     , blockContainsLines  = True
     , blockParagraph      = False
     , blockContinue       = \node -> do
             void (gobbleSpaces 4)
               <|> try (skipWhile isSpaceChar <* lookAhead lineEnd)
             pos <- getPosition
             return $! (pos, node)

     , blockConstructor    = \node ->
             return $! (addRange node
                        (codeBlock mempty
                           (getBlockText id node)))
     , blockFinalize       = \(Node cdata children) parent -> do
         -- strip off blank lines at end:
         let cdata' = cdata{ blockLines =
                                dropWhile isblankLine $ blockLines cdata }
         defaultFinalizer (Node cdata' children) parent
     }

isblankLine :: ((SourcePos, SourcePos), Text) -> Bool
isblankLine (_, t) =
  T.all (\c -> c == '\r' || c == '\n' || c == ' ' || c == '\t') t

fencedCodeSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
fencedCodeSpec = BlockSpec
     { blockType           = "FencedCode"
     , blockStart          = do
             prepos <- getPosition
             nonindentSpaces
             pos <- getPosition
             let indentspaces = unPos (sourceColumn pos) -
                                 unPos (sourceColumn prepos)
             (c, ticks) <-  (('`',) <$> some (char '`'))
                        <|> (('~',) <$> some (char '~'))
             let fencelength = length ticks
             guard $ fencelength >= 3
             skipWhile isSpaceChar
             let infoTok = satisfy (\d -> not (isLineEndChar d) &&
                                     (c == '~' || (d /= '`' && d /= '~')))
             info <- T.strip . unEntity . T.pack <$> many (pEscaped <|> infoTok)
             lookAhead $ void lineEnd <|> eof

             (content, attrs) <- parseFinalAttributes False info
                                  <|> (return $! (info, mempty))
             addNodeToStack $
                Node (defBlockData fencedCodeSpec){
                          blockData = toDyn
                               (c, fencelength, indentspaces,
                               content, attrs),
                          blockStartPos = [pos] } []
             return BlockStartMatch
     , blockCanContain     = const False
     , blockContainsLines  = True
     , blockParagraph      = False
     , blockContinue       = \node -> try (do
             let ((c, fencelength, _, _, _)
                    :: (Char, Int, Int, Text, Attributes)) = fromDyn
                                   (blockData (rootLabel node))
                                   ('`', 3, 0, mempty, mempty)
             nonindentSpaces
             pos <- getPosition
             t <- textWhile1 (== c)
             guard $ T.length t >= fencelength
             skipWhile isSpaceChar
             lookAhead $ void lineEnd <|> eof
             endOfBlock
             return $! (pos, node))
               <|> (do let ((_, _, indentspaces, _, _)
                              :: (Char, Int, Int, Text, Attributes)) = fromDyn
                                   (blockData (rootLabel node))
                                   ('`', 3, 0, mempty, mempty)
                       pos <- getPosition
                       _ <- gobbleUpToSpaces indentspaces
                       return $! (pos, node))
     , blockConstructor    = \node -> do
           let ((_, _, _, info, attrs) :: (Char, Int, Int, Text, Attributes)) =
                   fromDyn (blockData (rootLabel node)) ('`', 3, 0, mempty, mempty)
           let codetext = T.drop 1 (getBlockText id node)
           return $! addRange node $!
              if null attrs
                 then codeBlock info codetext
                 else addAttributes attrs $ codeBlock info codetext
     , blockFinalize       = defaultFinalizer
     }

rawHtmlSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
rawHtmlSpec = BlockSpec
     { blockType           = "RawHTML"
     , blockStart          = do
         pos <- getPosition
         (rawHtmlType, toks) <- withRaw $
           do nonindentSpaces
              char '<'
              ty <- choice $ map (\n -> n <$ startCond n) [1..7]
              -- some blocks can end on same line
              finished <- option False $ do
                 guard (ty /= 6 && ty /= 7)
                 endCond ty
                 return True
              when (ty == 7) $ do
                 -- type 7 blocks can't interrupt a paragraph
                 (n:_) <- nodeStack <$> getState
                 guard $ not $ blockParagraph (bspec n)
              skipWhile (not . isLineEndChar)
              -- we use 0 as a code to indicate that the block is closed
              return $! if finished then 0 else ty
         epos <- getPosition
         addNodeToStack $ Node (defBlockData rawHtmlSpec){
                      blockData = toDyn rawHtmlType,
                      blockLines = [((pos,epos),toks)],
                      blockStartPos = [pos] } []
         return BlockStartMatch
     , blockCanContain     = const False
     , blockContainsLines  = True
     , blockParagraph      = False
     , blockContinue       = \node@(Node ndata children) -> try $ do
         pos <- getPosition
         case fromDyn (blockData (rootLabel node)) (0 :: Int) of
              0 -> mzero  -- 0 means that the block start already closed
              6 -> (pos, node) <$ notFollowedBy blankLine
              7 -> (pos, node) <$ notFollowedBy blankLine
              n ->
                (do lookAhead (endCond n)
                    endOfBlock
                    toks <- textWhile1 (not . isLineEndChar)
                    endpos <- getPosition
                    le <- option mempty $ lookAhead lineEnd
                    return $! (pos, Node ndata{
                                    blockData = toDyn (0 :: Int)
                                  , blockLines = ((pos,endpos), toks <> le) :
                                                   blockLines ndata
                                  } children)) <|> (return $! (pos, node))
     , blockConstructor    = \node ->
             return $! (addRange node
                        (rawBlock (Format "html")
                           (getBlockText id node)))
     , blockFinalize       = defaultFinalizer
     }

---------------- for raw html:

startCond :: Monad m => Int -> BlockParser m il bl ()
startCond 1 = void $ try $ do
  textWhile1 isLetter >>= guard . isOneOfCI ["script","pre","style"]
  satisfy isSpaceChar <|> char '>' <|> lookAhead (satisfy isLineEndChar)
startCond 2 = void $ try $ do
  char '!'
  char '-'
  char '-'
startCond 3 = void $ char '?'
startCond 4 = void $ try $ do
  char '!'
  satisfy isAsciiUpper
startCond 5 = void $ try $ do
  char '!'
  char '['
  string "CDATA"
  char '['
startCond 6 = void $ try $ do
  optional (char '/')
  textWhile1 isAlphaNum >>=
    guard . isOneOfCI ["address", "article", "aside", "base",
    "basefont", "blockquote", "body", "caption", "center", "col",
    "colgroup", "dd", "details", "dialog", "dir", "div", "dl",
    "dt", "fieldset", "figcaption", "figure", "footer", "form", "frame",
    "frameset", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header",
    "hr", "html", "iframe", "legend", "li", "link", "main", "menu",
    "menuitem", "nav", "noframes", "ol", "optgroup", "option",
    "p", "param", "section", "source", "summary", "table", "tbody",
    "td", "tfoot", "th", "thead", "title", "tr", "track", "ul"]
  satisfy isSpaceChar
    <|> lookAhead (satisfy isLineEndChar)
    <|> char '>'
    <|> (char '/' >> char '>')
startCond 7 = void $ try $ do
  toks <- htmlOpenTag <|> htmlClosingTag
  guard $ not $ T.any isLineEndChar toks
  skipWhile isSpaceChar
  lookAhead lineEnd
startCond n = fail $ "Unknown HTML block type " ++ show n

endCond :: Monad m => Int -> BlockParser m il bl ()
endCond 1 = try $ do
  let closer = try $ do
        char '<'
        char '/'
        textWhile1 isAlphaNum >>= guard . isOneOfCI ["script","pre","style"]
        char '>'
  void $ skipManyTill (satisfy (not . isLineEndChar)) closer
endCond 2 = try $ do
  let closer = try $ char '-' >> char '-' >> char '>'
  void $ skipManyTill (satisfy (not . isLineEndChar)) closer
endCond 3 = try $ do
  let closer = try $ char '?' >> char '>'
  void $ skipManyTill (satisfy (not . isLineEndChar)) closer
endCond 4 = try $
  void $ skipManyTill (satisfy (not . isLineEndChar)) (char '>')
endCond 5 = try $ do
  let closer = try $ char ']' >> char ']' >> char '>'
  void $ skipManyTill (satisfy (not . isLineEndChar)) closer
endCond 6 = void blankLine
endCond 7 = void blankLine
endCond n = fail $ "Unknown HTML block type " ++ show n

--------------------------------

getBlockText :: (Text -> Text) -> BlockNode m il bl -> Text
getBlockText transform =
  T.concat . map transform . reverse . map snd . blockLines . rootLabel

removeIndent :: Text -> Text
removeIndent = T.dropWhile isSpaceChar

removeConsecutive :: [Int] -> [Int]
removeConsecutive (x:y:zs)
  | x == y + 1 = removeConsecutive (y:zs)
removeConsecutive xs = xs

-------------------------------------------------------------------------

collapseNodeStack :: [BlockNode m il bl] -> BlockParser m il bl (BlockNode m il bl)
collapseNodeStack [] = error "Empty node stack!"  -- should not happen
collapseNodeStack (n:ns) = foldM go n ns
  where go child parent
         = if blockCanContain (bspec parent) (bspec child)
              then blockFinalize (bspec child) child parent
              else error $ "collapseNodeStack: " ++
                     T.unpack (blockType (bspec parent)) ++
                     " cannot contain " ++ T.unpack (blockType (bspec child))

bspec :: BlockNode m il bl -> BlockSpec m il bl
bspec = blockSpec . rootLabel

endOfBlock :: Monad m => BlockParser m il bl ()
endOfBlock = updateState $ \st -> st{ blockMatched = False }

