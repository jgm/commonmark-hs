{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.Footnote
  ( footnoteSpec )
where
import Commonmark.Tokens
import Commonmark.Types
import Commonmark.Html
import Commonmark.Syntax
import Commonmark.Blocks
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.Util
import Commonmark.ReferenceMap
import Control.Monad (mzero)
import Data.Maybe (fromMaybe)
import Data.Dynamic
import Data.Tree
import Text.Parsec
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Semigroup (Semigroup(..))
import Data.Text.Lazy.Builder (Builder)

{-
TODO:
- create a way to keep a running counter
- on finalize, add the nodes to refmap
- parser for inline fn refs (this will use refmap)
  (inline parser can look this up and then just run
   blockConstructor on the contained nodes, or add
   a number, depending on format)
-}

data FootnoteDef m il bl =
  FootnoteDef Int [BlockNode m il bl]

footnoteSpec :: (Monad m, Typeable m, IsBlock il bl, IsInline il,
                 Typeable il, Typeable bl,
                 HasFootnoteRef il, HasFootnote bl)
             => SyntaxSpec m il bl
footnoteSpec = SyntaxSpec
  { syntaxBlockSpecs = [footnoteBlockSpec]
  , syntaxBracketedSpecs = []
  , syntaxFormattingSpecs = []
  , syntaxInlineParsers = [pFootnoteRef]
  }

footnoteBlockSpec :: (Monad m, Typeable m, Typeable il, Typeable bl,
                      IsBlock il bl, HasFootnote bl)
                  => BlockSpec m il bl
footnoteBlockSpec = BlockSpec
     { blockType           = "Footnote"
     , blockStart          = try $ do
             nonindentSpaces
             pos <- getPosition
             lab <- pLinkLabel
             _ <- symbol ':'
             lab' <- case T.uncons lab of
                          Just ('^', t') -> return t'
                          _ -> mzero
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
     , blockCanContain     = const True
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \n -> try $ do
             () <$ (gobbleSpaces 4)
               <|> (skipWhile (hasType Spaces) >> () <$ lookAhead lineEnd)
             pos <- getPosition
             return (pos, n)
     , blockConstructor    = \node -> do
          let (num, lab') = fromDyn (blockData (rootLabel node)) (1, mempty)
          (addRange node . footnote num lab' . mconcat) <$> mapM (\n ->
              blockConstructor (blockSpec (rootLabel n)) n)
            (reverse (subForest node))
     , blockFinalize       = \child@(Node bdata children) parent -> do
         let (num, lab') = fromDyn (blockData bdata) (1, mempty)
         updateState $ \s -> s{
           referenceMap = insertReference lab' (FootnoteDef num children)
                              (referenceMap s) }
         defaultFinalizer child parent
     }

pFootnoteRef :: (Monad m, HasFootnoteRef a) => InlineParser m a
pFootnoteRef = fail "undefined"

class HasFootnote a where
  footnote :: Int -> T.Text -> a -> a

instance HasFootnote Builder where
  -- footnote _ = mempty
  footnote num lab' x = "<footnote num=\""
    <> escapeHtml (T.pack (show num))
    <> "\" label=\"" <> escapeHtml lab' <> "\">\n"
    <> x <> "</footnote>\n"

instance (HasFootnote b, Monoid b)
        => HasFootnote (WithSourceMap b) where
  footnote num lab' x = (footnote num lab' <$> x) <* addName "footnote"

class HasFootnoteRef a where
  footnoteRef :: a -> a

instance HasFootnoteRef Builder where
  footnoteRef x = "<sup>" <> x <> "</sup>"

instance (HasFootnoteRef i, Monoid i)
        => HasFootnoteRef (WithSourceMap i) where
  footnoteRef x = (footnoteRef <$> x) <* addName "footnoteRef"
