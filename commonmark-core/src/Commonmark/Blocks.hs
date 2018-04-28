{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Commonmark.Blocks
  ( mkBlockParser
  , defaultBlockSpecs
  , BlockSpec(..)
  , BlockData(..)
  , defBlockData
  , BlockNode
  , BPState(..)
  , BlockParser
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
  -- * BlockSpecs
  , docSpec
  , indentedCodeSpec
  , fencedCodeSpec
  , blockQuoteSpec
  , atxHeaderSpec
  , setextHeaderSpec
  , thematicBreakSpec
  , listItemSpec
  , rawHtmlSpec
  , paraSpec
  )
where

import           Commonmark.HTML
import           Commonmark.Util
import           Commonmark.ReferenceMap
import           Commonmark.Inlines        (pEscaped, pLinkDestination,
                                            pLinkLabel, pLinkTitle, unEntity)
import           Commonmark.Tokens
import           Commonmark.Types
import           Control.Monad             (foldM, guard, mzero, void, unless,
                                            when)
import           Control.Monad.Trans.Class (lift)
import           Data.Char                 (isAsciiUpper, isDigit, isSpace)
import           Data.Dynamic
import           Data.List                 (findIndex, sort, partition)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Read            as TR
import           Data.Tree
import           Text.Parsec

mkBlockParser :: (Monad m, IsBlock il bl)
             => [BlockSpec m il bl] -- ^ Defines block syntax
             -> (ReferenceMap -> [Tok]
                  -> m (Either ParseError il)) -- ^ Inline parser
             -> [Tok] -- ^ Tokenized commonmark input
             -> m (Either ParseError bl)  -- ^ Result or error
mkBlockParser _ _ [] = return $ Right mempty
mkBlockParser specs ilParser (t:ts) =
  runParserT (setPosition (tokPos t) >> processLines specs)
          BPState{ referenceMap = mempty
                 , inlineParser = ilParser
                 , nodeStack    = [Node (defBlockData docSpec) []]
                 , blockMatched = False
                 , maybeLazy    = False
                 , maybeBlank   = True
                 }
          "source" (t:ts)

processLines :: (Monad m, IsBlock il bl)
             => [BlockSpec m il bl] -> BlockParser m il bl bl
processLines specs = do
  whileM_ (not . null <$> getInput) (processLine specs)
  tree <- (nodeStack <$> getState) >>= collapseNodeStack
  blockConstructor (blockSpec (rootLabel tree)) tree

processLine :: (Monad m, IsBlock il bl)
            => [BlockSpec m il bl] -> BlockParser m il bl ()
processLine specs = do
  -- check block continuations for each node in stack
  let checkContinue nd = do
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
  updateState $ \st -> st{ blockMatched = True
                         , maybeLazy = False
                         , maybeBlank = True }
  conts <- getState >>= mapM checkContinue . reverse . nodeStack
  let (contsmatched, contsunmatched) = partition fst conts
  let (matched, unmatched) = (map snd contsmatched, map snd contsunmatched)

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
  (skipMany1 (choice (map blockStart specs)) >> optional (blockStart paraSpec))
      <|>
    (do getState >>= guard . maybeLazy
        guard $ not isblank
        -- lazy line
        sp <- getPosition
        let addStartPos (Node bd cs) =
                Node bd{ blockStartPos = sp : blockStartPos bd } cs
        updateState $ \st -> st{ nodeStack =
             map addStartPos (reverse unmatched) ++ reverse matched })
      <|>
    blockStart paraSpec
      <|>
    return ()

  (cur:rest) <- nodeStack <$> getState
  -- add line contents
  (toks, endpos) <- restOfLine <|> (([],) <$> getPosition <* eof)
  -- add endpos
  let addEndPos (Node bdata children) = Node bdata{ blockEndPos =
                     endpos : blockEndPos bdata } children
  let curdata = rootLabel cur
  updateState $ \st -> st{
      nodeStack = map addEndPos $
        cur{ rootLabel =
               if blockContainsLines (bspec cur)
                  then curdata{ blockLines = toks : blockLines curdata }
                  else
                    if isblank &&
                       blockCanContain (bspec cur) paraSpec
                       then curdata{ blockBlanks = sourceLine endpos :
                                        blockBlanks curdata }
                       else curdata
           } : rest
      }
  -- showNodeStack

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

data BlockSpec m il bl = BlockSpec
     { blockType           :: Text
     , blockStart          :: BlockParser m il bl ()
     , blockCanContain     :: BlockSpec m il bl -> Bool
     , blockContainsLines  :: Bool
     , blockParagraph      :: Bool
     , blockContinue       :: BlockNode m il bl
                           -> BlockParser m il bl (SourcePos, BlockNode m il bl)
     , blockConstructor    :: BlockNode m il bl -> BlockParser m il bl bl
     , blockFinalize       :: BlockNode m il bl -> BlockNode m il bl
                           -> BlockParser m il bl (BlockNode m il bl)
     }

instance Show (BlockSpec m il bl) where
  show bs = "<BlockSpec " ++ T.unpack (blockType bs) ++ ">"

defaultBlockSpecs :: (Monad m, IsBlock il bl) => [BlockSpec m il bl]
defaultBlockSpecs =
    [ indentedCodeSpec
    , fencedCodeSpec
    , blockQuoteSpec
    , atxHeaderSpec
    , setextHeaderSpec
    , thematicBreakSpec
    , listItemSpec
    , rawHtmlSpec
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
     }
  deriving Show

defBlockData :: BlockSpec m il bl -> BlockData m il bl
defBlockData spec = BlockData
    { blockSpec      = spec
    , blockLines     = []
    , blockStartPos  = []
    , blockEndPos    = []
    , blockData      = toDyn ()
    , blockBlanks    = []
    }

type BlockNode m il bl = Tree (BlockData m il bl)

data BPState m il bl = BPState
     { referenceMap :: ReferenceMap
     , inlineParser :: ReferenceMap -> [Tok] -> m (Either ParseError il)
     , nodeStack    :: [BlockNode m il bl]   -- reverse order, head is tip
     , blockMatched :: Bool
     , maybeLazy    :: Bool
     , maybeBlank   :: Bool
     }

type BlockParser m il bl = ParsecT [Tok] (BPState m il bl) m

data ListData = ListData
     { listType    :: ListType
     , listSpacing :: ListSpacing
     } deriving (Show, Eq)

data ListItemData = ListItemData
     { listItemType   :: ListType
     , listItemIndent :: Int
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
     then updateState $ \st ->
            st{ nodeStack = node : cur : rest
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

docSpec :: (Monad m, Monoid bl) => BlockSpec m il bl
docSpec = BlockSpec
     { blockType           = "Doc"
     , blockStart          = mzero
     , blockCanContain     = const True
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \n -> (,n) <$> getPosition
     , blockConstructor    = \node ->
            mconcat <$> mapM (\n ->
                        blockConstructor (blockSpec (rootLabel n)) n)
                   (reverse (subForest node))
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
     , blockFinalize       = \child parent ->
         case parse ((,) <$> ((lookAhead anyTok >>= setPosition . tokPos) >>
                               many1 linkReferenceDef)
                         <*> getInput) ""
                  (getBlockText removeIndent child) of
               Left _ -> defaultFinalizer child parent
               Right (linkdefs, toks') -> do
                 mapM_
                   (\((_,lab),(dest,tit)) ->
                    updateState $ \st -> st{
                     referenceMap = insertReference lab (dest,tit)
                       (referenceMap st) }) linkdefs
                 let isRefPos = case toks' of
                                  (t:_) -> (< tokPos t)
                                  _     -> const False
                 let paranodes = if null toks'
                                    then []
                                    else [child{ rootLabel =
                                           (rootLabel child){
                                             blockLines = [toks'],
                                             blockStartPos =
                                               dropWhile isRefPos
                                                (blockStartPos
                                                 (rootLabel child)),
                                             blockEndPos =
                                               dropWhile isRefPos
                                                (blockEndPos
                                                 (rootLabel child)),
                                             blockSpec = paraSpec
                                             } }]
                 let thisnode = child{ rootLabel =
                        (rootLabel child){
                            blockData = toDyn linkdefs
                          , blockSpec = refLinkDefSpec
                        }}
                 return $ parent{ subForest = paranodes ++
                              thisnode : subForest parent }
     }

plainSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
plainSpec = paraSpec{
    blockConstructor    = \node ->
         (addRange node . plain)
             <$> runInlineParser (getBlockText removeIndent node)
  }


linkReferenceDef :: Parsec [Tok] s ((SourceRange, Text), (Text, Text))
linkReferenceDef = try $ do
  startpos <- getPosition
  lab <- pLinkLabel
  guard $ not $ T.all isSpace lab
  symbol ':'
  optional whitespace
  dest <- pLinkDestination
  guard $ not . null $ dest
  title <- option [] $ try $
             whitespace
             *> pLinkTitle
             <* skipWhile (hasType Spaces)
             <* lookAhead (void lineEnd <|> eof)
  skipWhile (hasType Spaces)
  endpos <- getPosition
  void lineEnd <|> eof
  return ((SourceRange [(startpos, endpos)], lab),
          (unEntity dest, unEntity title))

atxHeaderSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
atxHeaderSpec = BlockSpec
     { blockType           = "ATXHeader"
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
             addNodeToStack $ Node (defBlockData atxHeaderSpec){
                            blockLines = [raw'],
                            blockData = toDyn level,
                            blockStartPos = [pos] } []
     , blockCanContain     = const False
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = const mzero
     , blockConstructor    = \node ->
           (addRange node . header
     (fromDyn (blockData (rootLabel node)) 1))
  <$> runInlineParser (getBlockText removeIndent node)
     , blockFinalize       = defaultFinalizer
     }

setextHeaderSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
setextHeaderSpec = BlockSpec
     { blockType           = "SetextHeader"
     , blockStart          = try $ do
             (cur:rest) <- nodeStack <$> getState
             guard $ blockParagraph (bspec cur)
             nonindentSpaces
             pos <- getPosition
             level <- (2 :: Int) <$ skipMany1 (symbol '-')
                  <|> (1 :: Int) <$ skipMany1 (symbol '=')
             skipWhile (hasType Spaces)
             lookAhead (eof <|> void lineEnd)
             -- replace cur with new setext header node
             updateState $ \st ->
                st{ nodeStack = rest }
             addNodeToStack $
                  Node (rootLabel cur){
                          blockSpec  = setextHeaderSpec,
                          blockData = toDyn level,
                          blockStartPos =
                               blockStartPos (rootLabel cur) ++ [pos] } []

     , blockCanContain     = const False
     , blockContainsLines  = True
     , blockParagraph      = False
     , blockContinue       = const mzero
     , blockConstructor    = \node ->
           (addRange node . header
                 (fromDyn (blockData (rootLabel node)) 1))
             <$> runInlineParser (getBlockText removeIndent node)
     , blockFinalize       = defaultFinalizer
     }

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
          (addRange node . blockQuote . mconcat)
   <$> mapM (\n ->
              blockConstructor (blockSpec (rootLabel n)) n)
         (reverse (subForest node))
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
     , blockCanContain     = const True
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \node@(Node ndata children) -> do
             let lidata = fromDyn (blockData ndata)
                             (ListItemData undefined undefined)
             -- a marker followed by two blanks is just an empty item:
             guard $ null (blockBlanks ndata) ||
                     not (null children)
             pos <- getPosition
             gobbleSpaces (listItemIndent lidata) <|> 0 <$ lookAhead blankLine
             return (pos, node)
     , blockConstructor    = \node ->
          mconcat
   <$> mapM (\n ->
              blockConstructor (blockSpec (rootLabel n)) n)
         (reverse (subForest node))
     , blockFinalize       = defaultFinalizer
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
          let constructor n = blockConstructor (blockSpec (rootLabel n)) n
          (addRange node . list lt ls)
             <$> mapM constructor (reverse (subForest node))
     , blockFinalize       = \(Node cdata children) parent -> do
          let ListData lt _ = fromDyn (blockData cdata)
                                 (ListData undefined undefined)
          let removeConsecutive (x:y:zs)
                | x == y + 1 = removeConsecutive (y:zs)
              removeConsecutive xs = xs
          -- we might have any number of blanks at end of list,
          -- so we remove consecutive lines to get the last one
          -- in a series...
          let blanks = removeConsecutive
                       $ sort $ concatMap (getBlanks 1) children
          curline <- sourceLine <$> getPosition
          let ls = case blanks of
                         (l:_) | l < curline - 1 -> LooseList
                         _     -> TightList
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
          defaultFinalizer (Node cdata{ blockData = ldata' } children')
                           parent
     }

getBlanks :: Int -> BlockNode m il bl -> [Int]
getBlanks iteration (Node bdata cs) =
  let endline = case blockEndPos bdata of
                     []    -> 0
                     (p:_) -> sourceLine p
  in  (case blockBlanks bdata of
           (x:xs)
             | iteration == 1 -> x:xs
             | x == endline ->
               case findIndex (> 1) (zipWith (-) (x:xs) xs) of
                    Nothing -> x:xs
                    Just i  -> take i (x:xs)
           _ -> [])
        ++  concatMap
              (concatMap (getBlanks (iteration + 1)) . subForest)
              [c | c <- cs, blockType (bspec c) == "List"]

thematicBreakSpec :: (Monad m, IsBlock il bl)
            => BlockSpec m il bl
thematicBreakSpec = BlockSpec
     { blockType           = "ThematicBreak"
     , blockStart          = try $ do
             nonindentSpaces
             pos <- getPosition
             let tbchar c = symbol c <* skipWhile (hasType Spaces)
             cs <- choice $ map (many1 . tbchar) ['-', '_', '*']
             guard $ length cs >= 3
             void $ lookAhead lineEnd
             addNodeToStack $
                Node (defBlockData thematicBreakSpec){
                          blockStartPos = [pos] } []
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
             info <- unEntity <$>
                      many (pEscaped <|> noneOfToks [LineEnd, Symbol '`'])
             lookAhead $ void lineEnd <|> eof
             addNodeToStack $
                Node (defBlockData fencedCodeSpec){
                          blockData = toDyn
                               (c, fencelength, indentspaces, info),
                          blockStartPos = [pos] } []
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
             let ((_, _, _, info) :: (Char, Int, Int, Text)) =
                     fromDyn (blockData (rootLabel node)) ('`', 3, 0, mempty)
             return (addRange node
                        (codeBlock info
                            -- drop initial lineend token
                            (untokenize $ drop 1 (getBlockText id node))))
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
    "menuitem", "meta", "nav", "noframes", "ol", "optgroup", "option",
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

