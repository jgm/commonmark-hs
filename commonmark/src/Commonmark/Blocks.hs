{-# LANGUAGE CPP                   #-}
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
  , rawHtmlSpec
  , attributeSpec
  , paraSpec
  )
where

import           Commonmark.Tag
import           Commonmark.Util
import           Commonmark.ReferenceMap
import           Commonmark.Inlines        (pEscaped, pLinkDestination,
                                            pLinkLabel, pLinkTitle)
import           Commonmark.Entity         (unEntity)
import           Commonmark.Tokens
import           Commonmark.Types
import           Control.Monad             (foldM, guard, mzero, void, unless,
                                            when)
import           Control.Monad.Trans.Class (lift)
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid
#endif
import           Data.Char                 (isAsciiUpper, isDigit, isSpace)
import           Data.Dynamic
import           Data.List                 (sort)
import           Data.Text                 (Text)
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import qualified Data.Text.Read            as TR
import           Data.Tree
import           Text.Parsec

mkBlockParser :: (Monad m, IsBlock il bl)
             => [BlockSpec m il bl] -- ^ Defines block syntax
             -> [BlockParser m il bl bl] -- ^ Parsers to run at end
             -> (ReferenceMap -> [Tok]
                  -> m (Either ParseError il)) -- ^ Inline parser
             -> [BlockParser m il bl Attributes] -- ^ attribute parsers
             -> [Tok] -- ^ Tokenized commonmark input
             -> m (Either ParseError bl)  -- ^ Result or error
mkBlockParser _ _ _ _ [] = return $ Right mempty
mkBlockParser specs finalParsers ilParser attributeParsers (t:ts) =
  runParserT (setPosition (tokPos t) >> processLines specs finalParsers)
          BPState{ referenceMap     = emptyReferenceMap
                 , inlineParser     = ilParser
                 , nodeStack        = [Node (defBlockData docSpec) []]
                 , blockMatched     = False
                 , maybeLazy        = False
                 , maybeBlank       = True
                 , counters         = M.empty
                 , failurePositions = M.empty
                 , parseAttributes  = choice attributeParsers
                 , nextAttributes   = mempty
                 }
          "source" (t:ts)

processLines :: (Monad m, IsBlock il bl)
             => [BlockSpec m il bl]
             -> [BlockParser m il bl bl] -- ^ Parsers to run at end
             -> BlockParser m il bl bl
processLines specs finalParsers = do
  skipManyTill (processLine specs) eof
  tree <- (nodeStack <$> getState) >>= collapseNodeStack
  endContent <- mconcat <$> sequence finalParsers
  body <- blockConstructor (blockSpec (rootLabel tree)) tree
  return $ body <> endContent

processLine :: (Monad m, IsBlock il bl)
            => [BlockSpec m il bl] -> BlockParser m il bl ()
processLine specs = do
  -- check block continuations for each node in stack
  st' <- getState
  putState $! st'{ blockMatched = True
                 , maybeLazy = False
                 , maybeBlank = True
                 , failurePositions = M.empty }
  conts <- mapM checkContinue $ reverse (nodeStack st')
  let matched = [x | (True, x) <- conts]
  let unmatched = [x | (False, x) <- conts]

  -- if not everything matched, and last unmatched is paragraph,
  -- then we may have a lazy paragraph continuation
  case reverse unmatched of
         m:_ | blockParagraph (bspec m) ->
           updateState $ \st -> st{ maybeLazy = True }
         _ -> return ()

  -- close unmatched blocks
  if null unmatched
    then updateState $ \st -> st{ nodeStack = reverse matched }
         -- this update is needed or we lose startpos information
    else case reverse matched of
              []     -> error "no blocks matched"
              m:ms   -> do
                stack' <- (: ms) <$> collapseNodeStack (reverse unmatched ++ [m])
                updateState $ \st -> st{ nodeStack = stack' }

  isblank <- option False $ True <$ (do getState >>= guard . maybeBlank
                                        lookAhead blankLine)
  (skipMany1 (doBlockStarts specs) >> optional (blockStart paraSpec))
      <|>
    (do getState >>= guard . maybeLazy
        guard $ not isblank
        -- lazy line
        sp <- getPosition
        updateState $ \st -> st{ nodeStack =
             map (addStartPos sp) (reverse unmatched) ++ reverse matched })
      <|>
    void (blockStart paraSpec)
      <|>
    return ()

  (cur:rest) <- nodeStack <$> getState
  -- add line contents
  (toks, endpos) <- restOfLine
  let curdata = rootLabel cur
  updateState $ \st -> st{
      nodeStack = map (addEndPos endpos) $
        cur{ rootLabel =
               if blockContainsLines (bspec cur)
                  then curdata{ blockLines = toks : blockLines curdata }
                  else
                    if isblank
                       then curdata{ blockBlanks = sourceLine endpos :
                                        blockBlanks curdata }
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
doBlockStarts [] = mzero
doBlockStarts (spec:otherSpecs) = try $ do
  st' <- getState
  initPos <- getPosition
  case M.lookup (blockType spec) (failurePositions st') of
     Just pos' | initPos < pos' -> doBlockStarts otherSpecs
     _ -> (do
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
           doBlockStarts otherSpecs)
          <|> doBlockStarts otherSpecs


checkContinue :: Monad m
              => BlockNode m il bl
              -> BlockParser m il bl (Bool, BlockNode m il bl)
checkContinue nd = do
  ismatched <- blockMatched <$> getState
  if ismatched
     then
       (do (startpos, Node bdata children) <- blockContinue (bspec nd) nd
           matched' <- blockMatched <$> getState
           -- if blockContinue set blockMatched to False, it's
           -- because of characters on the line closing the block,
           -- so it's not to be counted as blank:
           unless matched' $
             updateState $ \st -> st{ maybeBlank = False }
           pos' <- if matched'
                      then return startpos
                      else getPosition
           return (matched',
                   Node bdata{ blockStartPos =
                     startpos : blockStartPos bdata,
                     blockEndPos =
                         if matched'
                            then blockEndPos bdata
                            else pos' : blockEndPos bdata
                     } children))
       <|> (False, nd) <$ updateState (\st -> st{
                              blockMatched = False })
     else return (False, nd)


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
  | BlockStartNoMatchBefore SourcePos
  deriving (Show, Eq)

-- | Defines a block-level element type.
data BlockSpec m il bl = BlockSpec
     { blockType           :: Text  -- ^ Descriptive name of block type
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
     , blockContainsLines  :: Bool -- ^ True if this kind of block
                           -- can contain text lines.
     , blockParagraph      :: Bool -- ^ True if this kind of block
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
    , listItemSpec
    , rawHtmlSpec
    , attributeSpec
    ]

defaultFinalizer :: BlockNode m il bl
                 -> BlockNode m il bl
                 -> BlockParser m il bl (BlockNode m il bl)
defaultFinalizer child parent =
  return $ parent{ subForest = child : subForest parent }

data BlockData m il bl = BlockData
     { blockSpec     :: BlockSpec m il bl
     , blockLines    :: [[Tok]]  -- in reverse order
     , blockStartPos :: [SourcePos]  -- in reverse order
     , blockEndPos   :: [SourcePos]  -- reverse order
     , blockData     :: Dynamic
     , blockBlanks   :: [Int]  -- non-content blank lines in block
     , blockAttributes :: Attributes
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
     { referenceMap     :: ReferenceMap
     , inlineParser     :: ReferenceMap -> [Tok] -> m (Either ParseError il)
     , nodeStack        :: [BlockNode m il bl]   -- reverse order, head is tip
     , blockMatched     :: !Bool
     , maybeLazy        :: !Bool
     , maybeBlank       :: !Bool
     , counters         :: M.Map Text Dynamic
     , failurePositions :: M.Map Text SourcePos  -- record known positions
                           -- where parsers fail to avoid repetition
     , parseAttributes  :: ParsecT [Tok] (BPState m il bl) m Attributes
     , nextAttributes   :: Attributes
     }

type BlockParser m il bl = ParsecT [Tok] (BPState m il bl) m

data ListData = ListData
     { listType    :: ListType
     , listSpacing :: ListSpacing
     } deriving (Show, Eq)

data ListItemData = ListItemData
     { listItemType         :: ListType
     , listItemIndent       :: Int
     , listItemBlanksInside :: Bool
     , listItemBlanksAtEnd  :: Bool
     } deriving (Show, Eq)

runInlineParser :: Monad m
                => [Tok]
                -> BlockParser m il bl il
runInlineParser toks = do
  refmap <- referenceMap <$> getState
  ilParser <- inlineParser <$> getState
  res <- lift $ ilParser refmap toks
  case res of
       Right ils -> return ils
       Left err  -> mkPT (\_ -> return (Empty (return (Error err))))
                    -- pass up ParseError

addRange :: (Monad m, IsBlock il bl)
         => BlockNode m il bl -> bl -> bl
addRange (Node b _)
 = ranged (SourceRange
            (reverse $ zip (blockStartPos b) (blockEndPos b)))

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
  return $ blockParagraph (bspec cur)

renderChildren :: (Monad m, IsBlock il bl)
               => BlockNode m il bl -> BlockParser m il bl [bl]
renderChildren node = mapM renderC $ reverse $ subForest node
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
         return $ mconcat $ map (\((range, lab), (dest, tit)) ->
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
  res <- lift $ runParserT ((,) <$> ((lookAhead anyTok >>= setPosition . tokPos) >>
                        many1 (linkReferenceDef (parseAttributes st)))
                  <*> getInput) st "" (getBlockText removeIndent node)
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
                                   (blockStartPos (rootLabel node)),
                                blockEndPos = dropWhile isRefPos
                                   (blockEndPos (rootLabel node))
                                }
                           }
          let refnode = node{ rootLabel =
                 (rootLabel node){
                     blockLines = takeWhile (any (isRefPos . tokPos))
                       (blockLines (rootLabel node))
                   , blockData = toDyn linkdefs
                   , blockSpec = refLinkDefSpec
                 }}
          return (node', Just refnode)

attributeSpec :: (Monad m, IsBlock il bl)
              => BlockSpec m il bl
attributeSpec = BlockSpec
     { blockType           = "Attribute"
     , blockStart          = do
         interruptsParagraph >>= guard . not
         pos <- getPosition
         pAttr <- parseAttributes <$> getState
         attrs <- pAttr
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
         pos <- getPosition
         pAttr <- parseAttributes <$> getState
         attrs <- pAttr
         skipWhile (hasType Spaces)
         lookAhead (void lineEnd <|> eof)
         let oldattrs = fromDyn (blockData (rootLabel n)) mempty :: Attributes
         let attrs' = oldattrs <> attrs
         return (pos, n{ rootLabel = (rootLabel n){
                          blockData = toDyn attrs' }})
     , blockConstructor    = \_ -> return mempty
     , blockFinalize       = \node parent -> do
         let attrs = fromDyn (blockData (rootLabel node)) mempty :: Attributes
         updateState $ \st -> st{ nextAttributes = attrs }
         defaultFinalizer node parent
     }

paraSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
paraSpec = BlockSpec
     { blockType           = "Paragraph"
     , blockStart          = try $ do
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
             return (pos, n)
     , blockConstructor    = \node ->
         (addRange node . paragraph)
             <$> runInlineParser (getBlockText removeIndent node)
     , blockFinalize       = \child parent -> do
         (mbchild, mbrefdefs) <- extractReferenceLinks child
         case (mbchild, mbrefdefs) of
           (_, Nothing) -> defaultFinalizer child parent
           (Nothing, Just refnode)
                        -> return parent{ subForest =
                                          refnode : subForest parent }
           (Just child', Just refnode)
                        -> return parent{ subForest =
                                        child' : refnode : subForest parent }
     }

plainSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
plainSpec = paraSpec{
    blockConstructor    = \node ->
         (addRange node . plain)
             <$> runInlineParser (getBlockText removeIndent node)
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
                        , linkAttributes = attrs })

atxHeadingSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
atxHeadingSpec = BlockSpec
     { blockType           = "ATXHeading"
     , blockStart          = try $ do
             nonindentSpaces
             pos <- getPosition
             hashes <- many1 (symbol '#')
             let level = length hashes
             guard $ level <= 6
             void spaceTok
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
         let toks = getBlockText removeIndent node
         (content, attr) <- parseFinalAttributes True toks <|> return (toks, mempty)
         ils <- runInlineParser content
         return $ (addRange node . addAttributes attr . heading level) ils
     , blockFinalize       = defaultFinalizer
     }

setextHeadingSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
setextHeadingSpec = BlockSpec
     { blockType           = "SetextHeading"
     , blockStart          = try $ do
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
         let toks = getBlockText removeIndent node
         (content, attr) <- parseFinalAttributes True toks <|> return (toks, mempty)
         ils <- runInlineParser content
         return $ (addRange node . addAttributes attr . heading level) ils
     , blockFinalize       = defaultFinalizer
     }

parseFinalAttributes :: Monad m
                     => Bool -> [Tok] -> BlockParser m il bl ([Tok], Attributes)
parseFinalAttributes requireWhitespace ts = do
  pAttr <- parseAttributes <$> getState
  let pAttr' = try $ (if requireWhitespace
                         then () <$ whitespace
                         else optional whitespace)
                     *> pAttr <* optional whitespace <* eof
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
     , blockStart          = try $ do
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
     , blockConstructor    = \node ->
          (addRange node . blockQuote . mconcat) <$> renderChildren node
     , blockFinalize       = defaultFinalizer
     }

listItemSpec :: (Monad m, IsBlock il bl) => BlockSpec m il bl
listItemSpec = BlockSpec
     { blockType           = "ListItem"
     , blockStart          = try $ do
             (pos, lidata) <- itemStart
             let linode = Node (defBlockData listItemSpec){
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
                            BulletList _    -> True
                            OrderedList 1 _ -> True
                            _               -> False
               notFollowedBy blankLine
             let curdata = fromDyn (blockData (rootLabel cur))
                                (ListData undefined undefined)
             let matchesList (BulletList c) (BulletList d)       = c == d
                 matchesList (OrderedList _ c) (OrderedList _ d) = c == d
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
             pos <- getPosition
             gobbleSpaces (listItemIndent lidata) <|> 0 <$ lookAhead blankLine
             return (pos, node)
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
                                   (l:_) -> l >= curline - 1
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

itemStart :: Monad m => BlockParser m il bl (SourcePos, ListItemData)
itemStart = do
  beforecol <- sourceColumn <$> getPosition
  gobbleUpToSpaces 3
  pos <- getPosition
  ty <- bulletListMarker <|> orderedListMarker
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
  return $ BulletList c

orderedListMarker :: Monad m => BlockParser m il bl ListType
orderedListMarker = do
  Tok WordChars _ ds <- satisfyWord (\t -> T.all isDigit t && T.length t < 10)
  (start :: Int) <- either fail (return . fst) (TR.decimal ds)
  Tok (Symbol delim) _ _ <- symbol '.' <|> symbol ')'
  return $ OrderedList start delim

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
                                 return $ curline - 1 : blockBlanks cdata
                             _ -> return $ blockBlanks cdata
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
     , blockStart          = try $ do
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
     , blockConstructor    = \node ->
             return (addRange node thematicBreak)
     , blockFinalize       = defaultFinalizer
     }

indentedCodeSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
indentedCodeSpec = BlockSpec
     { blockType           = "IndentedCode"
     , blockStart          = try $ do
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
             return (addRange node
                        (codeBlock mempty
                           (untokenize (getBlockText id node))))
     , blockFinalize       = \(Node cdata children) parent -> do
         -- strip off blank lines at end:
         let cdata' = cdata{ blockLines =
                                dropWhile isblankLine $ blockLines cdata }
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
     , blockStart          = try $ do
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
             info <- T.strip . unEntity <$> many (pEscaped <|> infoTok)
             lookAhead $ void lineEnd <|> eof
             addNodeToStack $
                Node (defBlockData fencedCodeSpec){
                          blockData = toDyn
                               (c, fencelength, indentspaces, info),
                          blockStartPos = [pos] } []
             return BlockStartMatch
     , blockCanContain     = const False
     , blockContainsLines  = True
     , blockParagraph      = False
     , blockContinue       = \node -> try (do
             let ((c, fencelength, _, _)
                    :: (Char, Int, Int, Text)) = fromDyn
                                   (blockData (rootLabel node))
                                   ('`', 3, 0, mempty)
             nonindentSpaces
             pos <- getPosition
             ts <- many1 (symbol c)
             guard $ length ts >= fencelength
             skipWhile (hasType Spaces)
             lookAhead $ void lineEnd <|> eof
             endOfBlock
             return (pos, node))
               <|> (do let ((_, _, indentspaces, _)
                              :: (Char, Int, Int, Text)) = fromDyn
                                   (blockData (rootLabel node))
                                   ('`', 3, 0, mempty)
                       pos <- getPosition
                       _ <- gobbleUpToSpaces indentspaces
                       return (pos, node))
     , blockConstructor    = \node -> do
           let ((_, _, _, info) :: (Char, Int, Int, T.Text)) =
                   fromDyn (blockData (rootLabel node)) ('`', 3, 0, mempty)
           let codetext = untokenize $ drop 1 (getBlockText id node)
           let infotoks = tokenize "info string" info
           -- drop 1 initial lineend token
           (content, attrs) <- parseFinalAttributes False infotoks <|> return (infotoks, mempty)
           return $ addRange node $
              if null attrs
                 then codeBlock info codetext
                 else addAttributes attrs $ codeBlock (untokenize content) codetext
     , blockFinalize       = defaultFinalizer
     }

rawHtmlSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
rawHtmlSpec = BlockSpec
     { blockType           = "RawHTML"
     , blockStart          = try $ do
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
              return $ if finished then 0 else ty
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
                    return (pos', Node ndata{
                                    blockData = toDyn (0 :: Int)
                                  , blockLines = (toks ++ le) : blockLines ndata
                                  } children)) <|> return (pos, node)
     , blockConstructor    = \node ->
             return (addRange node
                        (rawBlock (Format "html")
                           (untokenize (getBlockText id node))))
     , blockFinalize       = defaultFinalizer
     }

---------------- for raw html:

startCond :: Monad m => Int -> BlockParser m il bl ()
startCond 1 = void $ try $ do
  satisfyWord (isOneOfCI ["script","pre","style"])
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
                          Just (c, _) -> isAsciiUpper c
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
    "p", "param", "section", "source", "summary", "table", "tbody",
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
        satisfyWord (isOneOfCI ["script","pre","style"])
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

getBlockText :: ([Tok] -> [Tok]) -> BlockNode m il bl -> [Tok]
getBlockText transform =
  concatMap transform . reverse . blockLines . rootLabel

removeIndent :: [Tok] -> [Tok]
removeIndent = dropWhile (hasType Spaces)

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

