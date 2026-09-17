{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commonmark.Extensions.Footnote
  ( footnoteSpec
  , HasFootnote(..)
  )
where
import Commonmark.Tokens
import Commonmark.Types
import Commonmark.Html
import Commonmark.Syntax
import Commonmark.Blocks
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.TokParsers
import Commonmark.ReferenceMap
import Control.Monad.Trans.Class (lift)
import Control.Monad (mzero, foldM)
import Data.Graph (stronglyConnComp, flattenSCC)
import Data.List
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.Dynamic
import Data.Tree
import Text.Parsec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M

data FootnoteDef bl m =
  FootnoteDef Int Text [Text] (ReferenceMap -> m (Either ParseError bl))
  -- ^ number, label, labels of footnote references in the body
  -- (conservative approximation, used only for ordering the
  -- memoization pass in addFootnoteList), parser for contents
  deriving Typeable

instance Eq (FootnoteDef bl m) where
  FootnoteDef num1 lab1 _ _ == FootnoteDef num2 lab2 _ _
    = num1 == num2 && lab1 == lab2

instance Ord (FootnoteDef bl m) where
  (FootnoteDef num1 lab1 _ _) `compare` (FootnoteDef num2 lab2 _ _) =
    (num1, lab1) `compare` (num2, lab2)

-- | Memoized rendered contents of a footnote, stored in the reference
-- map (alongside the FootnoteDef) once the note has been rendered, so
-- that each note body is parsed only once no matter how many times it
-- is referenced.
data FootnoteRendered bl = FootnoteRendered Int Text bl
  deriving Typeable

-- | Marker inserted into the reference map passed to a note body's
-- parser while that note is being rendered.  A reference to an
-- in-progress note is a cycle; pFootnoteRef treats it as unresolved,
-- so it falls back to literal text instead of looping forever.
data FootnoteInProgress = FootnoteInProgress
  deriving Typeable

footnoteSpec :: (Monad m, Typeable m, IsBlock il bl, IsInline il,
                 Typeable il, Typeable bl, HasFootnote il bl)
             => SyntaxSpec m il bl
footnoteSpec = mempty
  { syntaxBlockSpecs = [footnoteBlockSpec]
  , syntaxInlineParsers = [withAttributes pFootnoteRef]
  , syntaxFinalParsers = [addFootnoteList]
  }

footnoteBlockSpec :: (Monad m, Typeable m, Typeable il, Typeable bl,
                      IsBlock il bl, IsInline il, HasFootnote il bl)
                  => BlockSpec m il bl
footnoteBlockSpec = BlockSpec
     { blockType           = "Footnote"
     , blockStart          = try $ do
             nonindentSpaces
             pos <- getPosition
             lab' <- pFootnoteLabel
             _ <- symbol ':'
             counters' <- counters <$> getState
             let num = fromMaybe (1 :: Int) $
                       M.lookup "footnote" counters' >>= fromDynamic
             updateState $ \s -> s{ counters =
                                     M.insert "footnote" (toDyn (num + 1))
                                      (counters s) }
             addNodeToStack $
                Node (defBlockData footnoteBlockSpec){
                            blockData = toDyn (num, lab')
                          , blockStartPos = [pos] } []
             return BlockStartMatch
     , blockCanContain     = const True
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \n -> try $ do
             () <$ (gobbleSpaces 4)
               <|> (skipWhile (hasType Spaces) >> () <$ lookAhead lineEnd)
             pos <- getPosition
             return $! (pos, n)
     , blockConstructor    = \node ->
          mconcat <$> mapM (\n ->
              blockConstructor (blockSpec (rootLabel n)) n)
           (subForest (reverseSubforests node))
     , blockFinalize       = \(Node root children) parent -> do
         let (num, lab') = fromDyn (blockData root) (1, mempty)
         st <- getState
         let mkNoteContents refmap =
               runParserT
                 (blockConstructor (blockSpec root) (Node root children))
                 st{ referenceMap = refmap }
                 "source" []
         let bodytoks = concatMap (concat . reverse . blockLines)
                          (flatten (Node root children))
         updateState $ \s -> s{
             referenceMap = insertReference lab'
                              (FootnoteDef num lab' (extractNoteRefs bodytoks)
                                mkNoteContents)
                              (referenceMap s)
             }
         return $! parent
     }

-- Conservatively extract the labels of footnote references occurring
-- in a note body.  These are used only to order the rendering of
-- notes in addFootnoteList so that referenced notes are rendered (and
-- memoized) before the notes that reference them; inaccuracies affect
-- only performance, not correctness.
extractNoteRefs :: [Tok] -> [Text]
extractNoteRefs toks =
  case parse (catMaybes <$> many ((Just <$> try pFootnoteLabel)
                                   <|> (Nothing <$ anyTok))) "" toks of
       Left _     -> []
       Right labs -> labs

pFootnoteLabel :: Monad m => ParsecT [Tok] u m Text
pFootnoteLabel = try $ do
  lab <- untokenize
      <$> try (between (symbol '[') (symbol ']')
            (snd <$> withRaw (many
              (pEscaped <|> noneOfToks [Symbol ']', Symbol '[', LineEnd]))))
  case T.uncons lab of
        Just ('^', t') | T.any (\x -> x /= ' ' && x /= '\t') t' && T.all (\x -> x /= '\n' && x /= '\r') t'
            -> return $! t'
        _ -> mzero

pFootnoteRef :: forall m a b.  (Monad m, Typeable m, Typeable a,
                 Typeable b, IsInline a, IsBlock a b, HasFootnote a b)
             => InlineParser m a
pFootnoteRef = try $ do
  lab <- pFootnoteLabel
  rm <- getReferenceMap
  case lookupReference lab rm :: Maybe FootnoteInProgress of
    Just _ -> mzero -- cyclic reference: leave it as literal text
    Nothing ->
      case lookupReference lab rm :: Maybe (FootnoteRendered b) of
        -- memoized contents (notes are pre-rendered in addFootnoteList,
        -- which runs before inline parsing of the main document):
        Just (FootnoteRendered num _ contents) -> return $!
          footnoteRef (T.pack (show num)) lab contents
        Nothing ->
          case lookupReference lab rm :: Maybe (FootnoteDef b m) of
            -- not yet rendered (only happens while another note that
            -- references this one is itself being rendered):
            Just (FootnoteDef num _ _ mkContents) -> do
              res <- lift . lift $ mkContents
                       (insertReference lab FootnoteInProgress rm)
              case res of
                   Left err -> mkPT (\_ -> return (Empty (return (Error err))))
                   Right contents -> return $!
                     footnoteRef (T.pack (show num)) lab contents
            Nothing -> mzero

addFootnoteList :: forall m il bl.
                   (Monad m, Typeable m, Typeable bl, HasFootnote il bl,
                    IsBlock il bl) => BlockParser m il bl bl
addFootnoteList = do
  rm <- referenceMap <$> getState
  let keys = M.keys . unReferenceMap $ rm
  let getNote key = lookupReference key rm :: Maybe (FootnoteDef bl m)
  let notes = sort $ mapMaybe getNote keys
  -- Render each note's contents exactly once, in dependency order
  -- (so that notes referenced by other notes are rendered, and
  -- memoized, first), caching the results in the reference map.
  -- Since final parsers run before inline parsing of the main
  -- document, every footnote reference outside of a note body will
  -- find the memoized contents.  Cyclic references are cut off by
  -- the FootnoteInProgress marker (see pFootnoteRef).
  let sccs = stronglyConnComp
        [ (def, normalizeLabel lab, map normalizeLabel refs)
        | def@(FootnoteDef _ lab refs _) <- notes ]
  let renderNote rm' (FootnoteDef num lab _ mkContents) = do
        res <- lift $ mkContents (insertReference lab FootnoteInProgress rm')
        case res of
             Left err -> mkPT (\_ -> return (Empty (return (Error err))))
             Right contents -> return $!
               insertReference lab (FootnoteRendered num lab contents) rm'
  rm' <- foldM renderNote rm (concatMap flattenSCC sccs)
  updateState $ \s -> s{ referenceMap = rm' }
  let renderedNote (FootnoteDef num lab _ _) = do
        FootnoteRendered _ _ contents <- lookupReference lab rm'
        return $! footnote num lab contents
  if null notes
     then return mempty
     else return $! footnoteList $ mapMaybe renderedNote notes

-- Must match the label normalization performed by insertReference
-- and lookupReference (see Commonmark.ReferenceMap).
normalizeLabel :: Text -> Text
normalizeLabel = T.toCaseFold . T.unwords . T.words

class IsBlock il bl => HasFootnote il bl | il -> bl where
  footnote :: Int -> Text -> bl -> bl
  footnoteList :: [bl] -> bl
  footnoteRef :: Text -> Text -> bl -> il

instance Rangeable (Html a) => HasFootnote (Html a) (Html a) where
  footnote num lab' x =
    addAttribute ("class", "footnote") $
    addAttribute ("id", "fn-" <> lab') $
    htmlBlock "div" $ Just $ htmlRaw "\n" <>
      (addAttribute ("class", "footnote-number") $
       htmlBlock "div" $ Just $ htmlRaw "\n" <>
        (addAttribute ("href", "#fnref-" <> lab') $
         htmlInline "a" (Just $ htmlText $ T.pack $ show num)) <>
         htmlRaw "\n") <>
      (addAttribute ("class", "footnote-contents") $
        htmlBlock "div" $ Just $ htmlRaw "\n" <> x)
  footnoteList items =
    addAttribute ("class", "footnotes") $
      htmlBlock "section" $ Just $ htmlRaw "\n" <> mconcat items
  footnoteRef x lab _ =
   addAttribute ("class", "footnote-ref") $
     htmlInline "sup" $ Just $
       addAttribute ("href", "#fn-" <> lab) $
       addAttribute ("id", "fnref-" <> lab) $
       htmlInline "a" $ Just (htmlText x)

instance (HasFootnote il bl, Semigroup bl, Semigroup il)
        => HasFootnote (WithSourceMap il) (WithSourceMap bl) where
  footnote num lab' x = (footnote num lab' <$> x) <* addName "footnote"
  footnoteList items = footnoteList <$> sequence items
  footnoteRef x y z = (footnoteRef x y <$> z) <* addName "footnoteRef"
