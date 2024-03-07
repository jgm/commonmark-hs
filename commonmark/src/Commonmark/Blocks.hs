{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE BangPatterns          #-}
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
  , addNodeToStack
  , collapseNodeStack
  , getBlockText
  , removeIndent
  , bspec
  , endOfBlock
  , interruptsParagraph
  , linkReferenceDef
  , renderChildren
  , reverseSubforests
  , getParentListType
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
  , plainSpec
  )
where

import           Commonmark.Tag
import           Commonmark.TokParsers
import           Commonmark.ReferenceMap
import           Commonmark.Inlines        (pEscapedSymbol, pLinkDestination,
                                            pLinkLabel, pLinkTitle)
import           Commonmark.Entity         (unEntity)
import           Commonmark.Tokens
import           Commonmark.Types
import           Control.Monad             (foldM, guard, mzero, void, unless,
                                            when)
import           Control.Monad.Trans.Class (lift)
import           Data.Foldable             (foldrM)
import           Unicode.Char              (isAsciiUpper, isAsciiLower, isDigit)
import           Unicode.Char.General.Compat (isSpace)
import           Data.Dynamic
import           Data.Text                 (Text)
import qualified Data.Map.Strict           as M
import qualified Data.Text                 as T
import qualified Data.Text.Read            as TR
import           Data.Tree
import           Text.Parsec
import Data.List (sort)

mkBlockParser
  :: (Monad m, IsBlock il bl)
  => [BlockSpec m il bl] -- ^ Defines block syntax
  -> [BlockParser m il bl bl] -- ^ Parsers to run at end
  -> (ReferenceMap -> [Tok] -> m (Either ParseError il)) -- ^ Inline parser
  -> [BlockParser m il bl Attributes] -- ^ attribute parsers
  -> [Tok] -- ^ Tokenized commonmark input
  -> m (Either ParseError bl)  -- ^ Result or error
mkBlockParser specs finalParsers ilParser attrParsers ts =
  runParserT (do case ts of
                   (t:_) -> setPosition (tokPos t)
                   []    -> return ()
                 processLines specs finalParsers)
          BPState{ referenceMap     = emptyReferenceMap
                 , inlineParser     = ilParser
                 , nodeStack        = [Node (defBlockData docSpec) []]
                 , blockMatched     = False
                 , maybeLazy        = True
                 , maybeBlank       = True
                 , counters         = M.empty
                 , failurePositions = M.empty
                 , attributeParsers = attrParsers
                 , nextAttributes   = mempty
                 }
          "source" (length ts `seq` ts)
          -- we evaluate length ts to make sure the list is
          -- fully evaluated; this helps performance.  note that
          -- we can't use deepseq because there's no instance for SourcePos.

processLines :: (Monad m, IsBlock il bl)
             => [BlockSpec m il bl]
             -> [BlockParser m il bl bl] -- ^ Parsers to run at end
             -> BlockParser m il bl bl
processLines specs finalParsers = {-# SCC processLines #-} do
  let go = eof <|> (processLine specs >> go) in go
  tree <- getState >>= collapseNodeStack . nodeStack
  updateState $ \st -> st{ nodeStack = [reverseSubforests tree] }
  endContent <- mconcat <$> sequence finalParsers
  tree':_ <- nodeStack <$> getState
  body <- blockConstructor (blockSpec (rootLabel tree')) tree'
  return $! body <> endContent

reverseSubforests :: Tree a -> Tree a
reverseSubforests (Node x cs) = Node x $ map reverseSubforests $ reverse cs

processLine :: (Monad m, IsBlock il bl)
            => [BlockSpec m il bl] -> BlockParser m il bl ()
processLine specs = do
  -- check block continuations for each node in stack
  st' <- getState
  putState $  st'{ blockMatched = True
                 , maybeLazy = True
                 , maybeBlank = True
                 , failurePositions = M.empty }
  (matched, unmatched) <-  foldrM checkContinue ([],[]) (nodeStack st')

  -- if not everything matched, and last unmatched is paragraph,
  -- then we may have a lazy paragraph continuation
  updateState $ \st -> st{ maybeLazy = maybeLazy st &&
     case unmatched of
          m:_ -> blockParagraph (bspec m)
          _   -> False }

  -- close unmatched blocks
  -- but first save state so we can revert if we have a lazy line
  revertState <- getState
  if null unmatched
    then updateState $ \st -> st{ nodeStack = matched }
         -- this update is needed or we lose startpos information
    else case matched of
              []   -> error "no blocks matched"
              m:ms -> do
                m' <- collapseNodeStack (unmatched ++ [m])
                updateState $ \st -> st{ nodeStack = m':ms }

  restBlank <- option False $ True <$ lookAhead blankLine

  {-# SCC block_starts #-} unless restBlank $
    (do skipMany1 (doBlockStarts specs)
        optional (try (blockStart paraSpec)))
      <|>
    (do getState >>= guard . maybeLazy
        -- lazy line
        sp <- getPosition
        updateState $ const revertState
        updateState $ \st -> st{ nodeStack =
             map (addStartPos sp) (nodeStack st) })
      <|>
    void (try (blockStart paraSpec))
      <|>
    return ()

  (cur:rest) <- nodeStack <$> getState
  -- add line contents
  let curdata = rootLabel cur
  when (blockParagraph (bspec cur)) $ skipMany spaceTok
  pos <- getPosition
  toks <- {-# SCC restOfLine #-} restOfLine
  updateState $ \st -> st{
      nodeStack =
        cur{ rootLabel =
               if blockContainsLines (bspec cur)
                  then curdata{ blockLines = toks : blockLines curdata }
                  else
                    if maybeBlank st && restBlank
                       then curdata{ blockBlanks = sourceLine pos :
                                        blockBlanks curdata }
                       else curdata
           } : rest
      }
  -- showNodeStack

addStartPos :: SourcePos -> BlockNode m il bl -> BlockNode m il bl
addStartPos sp (Node bd cs) = Node bd{ blockStartPos = sp : blockStartPos bd } cs

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
             updateState $ \st -> st{ maybeBlank = False,
                                      maybeLazy = False }
           let new = Node bdata{ blockStartPos =
                      startpos : blockStartPos bdata
                      } children
           return $!
             if matched'
                then (new:matched, unmatched)
                else (matched, new:unmatched))
       <|> (matched, nd:unmatched) <$ updateState (\st -> st{
                                         blockMatched = False })
     else return (matched, nd:unmatched)


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
     , blockLines      :: [[Tok]]  -- in reverse order
     , blockStartPos   :: [SourcePos]  -- in reverse order
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
    , blockData     = toDyn ()
    , blockBlanks   = []
    , blockAttributes = mempty
    }

type BlockNode m il bl = Tree (BlockData m il bl)

data BPState m il bl = BPState
     { referenceMap     :: !ReferenceMap
     , inlineParser     :: ReferenceMap -> [Tok] -> m (Either ParseError il)
     , nodeStack        :: [BlockNode m il bl]   -- reverse order, head is tip
     , blockMatched     :: !Bool
     , maybeLazy        :: !Bool
     , maybeBlank       :: !Bool
     , counters         :: M.Map Text Dynamic
     , failurePositions :: M.Map Text SourcePos  -- record known positions
                           -- where parsers fail to avoid repetition
     , attributeParsers :: [ParsecT [Tok] (BPState m il bl) m Attributes]
     , nextAttributes   :: !Attributes
     }

type BlockParser m il bl = ParsecT [Tok] (BPState m il bl) m

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

-- | Get type of the enclosing List block. If the parent isn't
-- a List block, return Nothing.
getParentListType :: Monad m => BlockParser m il bl (Maybe ListType)
getParentListType = do
  (cur:_) <- nodeStack <$> getState
  if blockType (bspec cur) == "List"
     then do
       let ListData lt _ = fromDyn (blockData (rootLabel cur))
                            (ListData (BulletList '*') TightList)
       return $ Just lt
     else return Nothing

runInlineParser :: Monad m
                => [Tok]
                -> BlockParser m il bl il
runInlineParser toks = {-# SCC runInlineParser #-} do
  refmap <- referenceMap <$> getState
  ilParser <- inlineParser <$> getState
  res <- lift $ ilParser refmap toks
  case res of
       Right ils -> return $! ils
       Left err  -> mkPT (\_ -> return (Empty (return (Error err))))
                    -- pass up ParseError

addRange :: (Monad m, IsBlock il bl)
         => BlockNode m il bl -> bl -> bl
addRange (Node b _)
 = ranged (SourceRange
            (go . reverse $ map (\pos ->
                                  (pos, setSourceColumn
                                         (incSourceLine pos 1) 1))
                                (blockStartPos b)))
   where
     go [] = []
     go ((!startpos1, !endpos1):(!startpos2, !endpos2):rest)
       | startpos1 == startpos2
       , endpos1 == endpos2   = go ((startpos1, endpos2):rest)
       | endpos1 == startpos2 = go ((startpos1, endpos2):rest)
     go (x:xs) = x : go xs

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
          else addAttributes attrs) .
        addRange n <$> blockConstructor (blockSpec (rootLabel n)) n

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
                  undefined :: [((SourceRange, Text), LinkInfo)]
         return $! mconcat $ map (\((range, lab), linkinfo) ->
            ranged range
              (addAttributes (linkAttributes linkinfo)
                (referenceLinkDefinition lab (linkDestination linkinfo,
                                            linkTitle linkinfo)))) linkdefs
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
  res <- lift $ runParserT ((,) <$> ((lookAhead anyTok >>= setPosition . tokPos) >>
                        many1 (linkReferenceDef (choice $ attributeParsers st)))
                  <*> getInput) st "" (getBlockText node)
  case res of
        Left _ -> return (Just node, Nothing)
        Right (linkdefs, toks') -> do
          mapM_
            (\((_,lab),linkinfo) ->
             updateState $ \s -> s{
              referenceMap = insertReference lab linkinfo
                (referenceMap s) }) linkdefs
          let isRefPos = case toks' of
                           (t:_) -> (< tokPos t)
                           _     -> const False
          let node' = if null toks'
                         then Nothing
                         else Just node{ rootLabel =
                              (rootLabel node){
                                blockLines = [toks'],
                                blockStartPos = dropWhile isRefPos
                                   (blockStartPos (rootLabel node))
                                }
                           }
          let refnode = node{ rootLabel =
                 (rootLabel node){
                     blockLines = takeWhile (any (isRefPos . tokPos))
                       (blockLines (rootLabel node))
                   , blockStartPos = takeWhile isRefPos
                       (blockStartPos (rootLabel node))
                   , blockData = toDyn linkdefs
                   , blockSpec = refLinkDefSpec
                 }}
          return (node', Just refnode)

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
         skipWhile (hasType Spaces)
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
         skipWhile (hasType Spaces)
         lookAhead (void lineEnd <|> eof)
         let oldattrs = fromDyn (blockData (rootLabel n)) mempty :: Attributes
         let attrs' = oldattrs <> attrs
         return  (pos, n{ rootLabel = (rootLabel n){
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
             skipWhile (hasType Spaces)
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
             skipWhile (hasType Spaces)
             pos <- getPosition
             notFollowedBy lineEnd
             return $! (pos, n)
     , blockConstructor    = \node ->
         paragraph <$> runInlineParser (getBlockText node)
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
         plain <$> runInlineParser (getBlockText node)
  }


linkReferenceDef :: Monad m
                 => ParsecT [Tok] s m Attributes
                 -> ParsecT [Tok] s m ((SourceRange, Text), LinkInfo)
linkReferenceDef attrParser = try $ do
  startpos <- getPosition
  lab <- pLinkLabel
  guard $ not $ T.all isSpace lab
  symbol ':'
  optional whitespace
  linkpos <- getPosition
  dest <- pLinkDestination
  (title, attrs) <- option (mempty, mempty) $ try $ do
             tit <- option mempty $ try (whitespace *> pLinkTitle)
             skipWhile (hasType Spaces)
             as <- option mempty attrParser
             skipWhile (hasType Spaces)
             lookAhead (void lineEnd <|> eof)
             return (tit, as)
  endpos <- getPosition
  void lineEnd <|> eof
  return ((SourceRange [(startpos, endpos)], lab),
                LinkInfo{ linkDestination = unEntity dest
                        , linkTitle = unEntity title
                        , linkAttributes = attrs
                        , linkPos = Just linkpos })

atxHeadingSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
atxHeadingSpec = BlockSpec
     { blockType           = "ATXHeading"
     , blockStart          = do
             nonindentSpaces
             pos <- getPosition
             hashes <- many1 (symbol '#')
             let level = length hashes
             guard $ level <= 6
             (spaceTok *> skipMany spaceTok)
                <|> void (lookAhead lineEnd)
                <|> lookAhead eof
             raw <- many (satisfyTok (not . hasType LineEnd))
             -- trim off closing ###
             let removeClosingHash (_ :: Int) [] = []
                 removeClosingHash 0 (Tok Spaces _ _ : xs) =
                   removeClosingHash 0 xs
                 removeClosingHash _ (Tok (Symbol '#') _ _ :
                                      Tok (Symbol '\\') _ _ : _) =
                   reverse raw
                 removeClosingHash _ (Tok (Symbol '#') _ _ : xs) =
                   removeClosingHash 1 xs
                 removeClosingHash 1 (Tok Spaces _ _ : xs) = xs
                 removeClosingHash 1 (x:_)
                  | tokType x /= Symbol '#' = reverse raw
                 removeClosingHash _ xs = xs
             let raw' = reverse . removeClosingHash 0 . reverse $ raw
             addNodeToStack $ Node (defBlockData atxHeadingSpec){
                            blockLines = [raw'],
                            blockData = toDyn level,
                            blockStartPos = [pos] } []
             return BlockStartMatch
     , blockCanContain     = const False
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = const mzero
     , blockConstructor    = \node -> do
         let level = fromDyn (blockData (rootLabel node)) 1
         ils <- runInlineParser (getBlockText node)
         return $! heading level ils
     , blockFinalize       = \node@(Node cdata children) parent -> do
         let oldAttr = blockAttributes cdata
         let toks = getBlockText node
         (newtoks, attr) <- parseFinalAttributes True toks
                        <|> (return (toks, mempty))
         defaultFinalizer (Node cdata{ blockAttributes = oldAttr <> attr
                                     , blockLines = [newtoks] }
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
             level <- (2 :: Int) <$ skipMany1 (symbol '-')
                  <|> (1 :: Int) <$ skipMany1 (symbol '=')
             skipWhile (hasType Spaces)
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
                                   blockStartPos (rootLabel cur') ++ [pos] }
                                    []
                 return BlockStartMatch
     , blockCanContain     = const False
     , blockContainsLines  = True
     , blockParagraph      = False
     , blockContinue       = const mzero
     , blockConstructor    = \node -> do
         let level = fromDyn (blockData (rootLabel node)) 1
         ils <- runInlineParser (getBlockText node)
         return $! heading level ils
     , blockFinalize       = \node@(Node cdata children) parent -> do
         let oldAttr = blockAttributes cdata
         let toks = getBlockText node
         (newtoks, attr) <- parseFinalAttributes True toks
                        <|> (return (toks, mempty))
         defaultFinalizer (Node cdata{ blockAttributes = oldAttr <> attr
                                     , blockLines = [newtoks] }
                                children) parent
     }

parseFinalAttributes :: Monad m
                     => Bool -> [Tok] -> BlockParser m il bl ([Tok], Attributes)
parseFinalAttributes requireWhitespace ts = do
  attrParsers <- attributeParsers <$> getState
  let pAttr' = try $ (if requireWhitespace
                         then () <$ whitespace
                         else optional whitespace)
                     *> choice attrParsers <* optional whitespace <* eof
  st <- getState
  res <- lift $ runParserT
       ((,) <$> many (notFollowedBy pAttr' >> anyTok)
            <*> option [] pAttr') st "heading contents" ts
  case res of
    Left _         -> mzero
    Right (xs, ys) -> return (xs, ys)

blockQuoteSpec :: (Monad m, IsBlock il bl) => BlockSpec m il bl
blockQuoteSpec = BlockSpec
     { blockType           = "BlockQuote"
     , blockStart          = do
             nonindentSpaces
             pos <- getPosition
             _ <- symbol '>'
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
             _ <- symbol '>'
             _ <- gobbleUpToSpaces 1
             return (pos, n)
     , blockConstructor    = fmap (blockQuote . mconcat) . renderChildren
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
                                (ListData (BulletList '*') TightList)
             let isSingleRomanDigit n = n == 1 || n == 5 || n == 10 ||
                                        n == 50 || n == 100 || n == 500 ||
                                        n == 1000
             let matchesOrderedListStyle
                  (OrderedList _s1 e1 d1) (OrderedList s2 e2 d2) =
                    d1 == d2 && -- roman can match alphabetic if single-digit:
                      case (e1, e2) of
                        (LowerAlpha, LowerRoman) -> isSingleRomanDigit s2
                        (UpperAlpha, UpperRoman) -> isSingleRomanDigit s2
                        (LowerRoman, LowerAlpha) -> isSingleRomanDigit s2
                        (UpperRoman, UpperAlpha) -> isSingleRomanDigit s2
                        _ -> e1 == e2
                 matchesOrderedListStyle _ _ = False

             let matchesList (BulletList c) (BulletList d)       = c == d
                 matchesList x@OrderedList{}
                             y@OrderedList{} = matchesOrderedListStyle x y
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
                             (ListItemData (BulletList '*') 0 False False)
             -- a marker followed by two blanks is just an empty item:
             pos <- getPosition
             case blockBlanks ndata of
                  _:_ | null children -> lookAhead blankLine
                  _ -> () <$ gobbleSpaces (listItemIndent lidata) <|> lookAhead blankLine
             return (pos, node)
     , blockConstructor    = fmap mconcat . renderChildren
     , blockFinalize       = \(Node cdata children) parent -> do
          let lidata = fromDyn (blockData cdata)
                                 (ListItemData (BulletList '*')
                                   0 False False)
          let allblanks = reverse . sort . concat $ blockBlanks cdata :
                                  map (blockBlanks . rootLabel)
                                  (filter ((== "List") . blockType .
                                   blockSpec . rootLabel) children)
          curline <- sourceLine <$> getPosition
          let blanksAtEnd = case allblanks of
                                   (l:_) -> l >= curline - 1
                                   _     -> False
          let blanksInside = case length (removeConsecutive allblanks) of
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
  return (pos, ListItemData{
           listItemType = ty
          , listItemIndent = (aftercol - beforecol) + numspaces
          , listItemBlanksInside = False
          , listItemBlanksAtEnd = False
          })

bulletListMarker :: Monad m => BlockParser m il bl ListType
bulletListMarker = do
  Tok (Symbol c) _ _ <- symbol '-' <|> symbol '*' <|> symbol '+'
  return $! BulletList c

orderedListMarker :: Monad m => BlockParser m il bl ListType
orderedListMarker = do
  Tok WordChars _ ds <- satisfyWord (\t -> T.all isDigit t && T.length t < 10)
  (start :: Int) <- either fail (return . fst) (TR.decimal ds)
  delimtype <- Period <$ symbol '.' <|> OneParen <$ symbol ')'
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
                                 (ListData (BulletList '*') TightList)
          list lt ls <$> renderChildren node
     , blockFinalize       = \(Node cdata children) parent -> do
          let ListData lt _ = fromDyn (blockData cdata)
                                 (ListData (BulletList '*') TightList)
          let getListItemData (Node d _) =
                fromDyn (blockData d)
                  (ListItemData (BulletList '*') 0 False False)
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
                                 return $! case blockBlanks cdata of
                                    lb:b | lb == curline - 1 ->
                                        lb:b
                                    b ->
                                       curline - 1 : b
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
            Tok (Symbol c) _ _ <- symbol '-'
                              <|> symbol '_'
                              <|> symbol '*'
            skipWhile (hasType Spaces)
            let tbchar = symbol c <* skipWhile (hasType Spaces)
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
     , blockConstructor    = \_ -> return thematicBreak
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
               <|> try (skipWhile (hasType Spaces) <* lookAhead lineEnd)
             pos <- getPosition
             return (pos, node)

     , blockConstructor    = \node ->
             return $! codeBlock mempty (untokenize (getBlockText node))
     , blockFinalize       = \(Node cdata children) parent -> do
         -- strip off blank lines at end:
         let blanks = takeWhile isblankLine $ blockLines cdata
         let numblanks = length blanks
         let cdata' = cdata{ blockLines =
                                drop numblanks $ blockLines cdata
                           , blockStartPos =
                                drop numblanks $ blockStartPos cdata
                           }
         defaultFinalizer (Node cdata' children) parent
     }

isblankLine :: [Tok] -> Bool
isblankLine []                    = True
isblankLine [Tok LineEnd _ _]     = True
isblankLine (Tok Spaces _ _ : xs) = isblankLine xs
isblankLine _                     = False

fencedCodeSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
fencedCodeSpec = BlockSpec
     { blockType           = "FencedCode"
     , blockStart          = do
             prepos <- getPosition
             nonindentSpaces
             pos <- getPosition
             let indentspaces = sourceColumn pos - sourceColumn prepos
             (c, ticks) <-  (('`',) <$> many1 (symbol '`'))
                        <|> (('~',) <$> many1 (symbol '~'))
             let fencelength = length ticks
             guard $ fencelength >= 3
             skipWhile (hasType Spaces)
             let infoTok = noneOfToks (LineEnd : [Symbol '`' | c == '`'])
             info <- T.strip . unEntity <$> many (pEscapedSymbol <|> infoTok)
             lookAhead $ void lineEnd <|> eof

             let infotoks = tokenize "info string" info
             (content, attrs) <- parseFinalAttributes False infotoks
                                  <|> (return (infotoks, mempty))
             addNodeToStack $
                Node (defBlockData fencedCodeSpec){
                          blockData = toDyn
                               (c, fencelength, indentspaces,
                               untokenize content, attrs),
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
             ts <- many1 (symbol c)
             guard $ length ts >= fencelength
             skipWhile (hasType Spaces)
             lookAhead $ void lineEnd <|> eof
             endOfBlock
             return $! (pos, node))
               <|> (do let ((_, _, indentspaces, _, _)
                              :: (Char, Int, Int, Text, Attributes)) = fromDyn
                                   (blockData (rootLabel node))
                                   ('`', 3, 0, mempty, mempty)
                       pos <- getPosition
                       _ <- gobbleUpToSpaces indentspaces
                       return (pos, node))
     , blockConstructor    = \node -> do
           let ((_, _, _, info, attrs) :: (Char, Int, Int, Text, Attributes)) =
                   fromDyn (blockData (rootLabel node)) ('`', 3, 0, mempty, mempty)
           let codetext = untokenize $ drop 1 (getBlockText node)
           return $!
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
              symbol '<'
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
              skipWhile (not . hasType LineEnd)
              -- we use 0 as a code to indicate that the block is closed
              return $! if finished then 0 else ty
         addNodeToStack $ Node (defBlockData rawHtmlSpec){
                      blockData = toDyn rawHtmlType,
                      blockLines = [toks],
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
                (do pos' <- getPosition
                    lookAhead (endCond n)
                    endOfBlock
                    toks <- many (satisfyTok (not . hasType LineEnd))
                    le <- option [] $ (:[]) <$> lookAhead lineEnd
                    return $! (pos', Node ndata{
                                    blockData = toDyn (0 :: Int)
                                  , blockLines = (toks ++ le) : blockLines ndata
                                  } children)) <|> (return (pos, node))
     , blockConstructor    = \node ->
             return $! rawBlock (Format "html")
                           (untokenize (getBlockText node))
     , blockFinalize       = defaultFinalizer
     }

---------------- for raw html:

startCond :: Monad m => Int -> BlockParser m il bl ()
startCond 1 = void $ try $ do
  satisfyWord (isOneOfCI ["script","pre","style","textarea"])
  spaceTok
     <|> symbol '>'
     <|> lookAhead lineEnd
startCond 2 = void $ try $ do
  symbol '!'
  symbol '-'
  symbol '-'
startCond 3 = void $ symbol '?'
startCond 4 = void $ try $ do
  symbol '!'
  satisfyWord (\t -> case T.uncons t of
                          Just (c, _) -> isAsciiLetter c
                          _           -> False)
startCond 5 = void $ try $ do
  symbol '!'
  symbol '['
  satisfyWord (== "CDATA")
  symbol '['
startCond 6 = void $ try $ do
  optional (symbol '/')
  satisfyWord (isOneOfCI ["address", "article", "aside", "base",
    "basefont", "blockquote", "body", "caption", "center", "col",
    "colgroup", "dd", "details", "dialog", "dir", "div", "dl",
    "dt", "fieldset", "figcaption", "figure", "footer", "form", "frame",
    "frameset", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header",
    "hr", "html", "iframe", "legend", "li", "link", "main", "menu",
    "menuitem", "nav", "noframes", "ol", "optgroup", "option",
    "p", "param", "search", "section", "summary", "table", "tbody",
    "td", "tfoot", "th", "thead", "title", "tr", "track", "ul"])
  spaceTok
    <|> lookAhead lineEnd
    <|> symbol '>'
    <|> (symbol '/' >> symbol '>')
startCond 7 = void $ try $ do
  toks <- htmlOpenTag <|> htmlClosingTag
  guard $ not $ any (hasType LineEnd) toks
  skipWhile (hasType Spaces)
  lookAhead lineEnd
startCond n = fail $ "Unknown HTML block type " ++ show n

endCond :: Monad m => Int -> BlockParser m il bl ()
endCond 1 = try $ do
  let closer = try $ do
        symbol '<'
        symbol '/'
        satisfyWord (isOneOfCI ["script","pre","style","textarea"])
        symbol '>'
  skipManyTill (satisfyTok (not . hasType LineEnd)) closer
endCond 2 = try $ do
  let closer = try $ symbol '-' >> symbol '-' >> symbol '>'
  skipManyTill (satisfyTok (not . hasType LineEnd)) closer
endCond 3 = try $ do
  let closer = try $ symbol '?' >> symbol '>'
  skipManyTill (satisfyTok (not . hasType LineEnd)) closer
endCond 4 = try $
  skipManyTill (satisfyTok (not . hasType LineEnd)) (symbol '>')
endCond 5 = try $ do
  let closer = try $ symbol ']' >> symbol ']' >> symbol '>'
  skipManyTill (satisfyTok (not . hasType LineEnd)) closer
endCond 6 = void blankLine
endCond 7 = void blankLine
endCond n = fail $ "Unknown HTML block type " ++ show n

--------------------------------

getBlockText :: BlockNode m il bl -> [Tok]
getBlockText =
  concat . reverse . blockLines . rootLabel

removeIndent :: [Tok] -> [Tok]
removeIndent = dropWhile (hasType Spaces)

removeConsecutive :: [Int] -> [Int]
removeConsecutive (x:y:zs)
  | x == y + 1 = removeConsecutive (y:zs)
removeConsecutive xs = xs

isAsciiLetter :: Char -> Bool
isAsciiLetter c =
  isAsciiUpper c || isAsciiLower c

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

