{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.Footnote
  ( footnoteSpec )
where
import Commonmark.Types
import Commonmark.Syntax
import Commonmark.Blocks
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.Util
import Control.Monad (mzero)
import Data.Tree
import Text.Parsec
import qualified Data.Text as T
import Data.Semigroup (Semigroup(..))
import Data.Text.Lazy.Builder (Builder)

footnoteSpec :: (Monad m, IsBlock il bl, IsInline il,
                 HasFootnoteRef il, HasFootnote bl)
             => SyntaxSpec m il bl
footnoteSpec = SyntaxSpec
  { syntaxBlockSpecs = [footnoteBlockSpec]
  , syntaxBracketedSpecs = []
  , syntaxFormattingSpecs = []
  , syntaxInlineParsers = [pFootnoteRef]
  }

footnoteBlockSpec :: (Monad m, IsBlock il bl, HasFootnote bl)
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
             addNodeToStack $
                Node (defBlockData footnoteBlockSpec){
                          blockStartPos = [pos] } []
     , blockCanContain     = const True
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \n -> try $ do
             _ <- gobbleSpaces 4
             pos <- getPosition
             return (pos, n)
     , blockConstructor    = \node ->
          (addRange node . footnote . mconcat) <$> mapM (\n ->
              blockConstructor (blockSpec (rootLabel n)) n)
         (reverse (subForest node))
     , blockFinalize       = \child parent -> do
         -- TODO add to referenceMap
         defaultFinalizer child parent
     }

pFootnoteRef :: (Monad m, HasFootnoteRef a) => InlineParser m a
pFootnoteRef = fail "undefined"

class HasFootnote a where
  footnote :: a -> a

instance HasFootnote Builder where
  -- footnote _ = mempty
  footnote x = "<footnote>" <> x <> "</footnote>"

instance (HasFootnote b, Monoid b)
        => HasFootnote (WithSourceMap b) where
  footnote x = (footnote <$> x) <* addName "footnote"

class HasFootnoteRef a where
  footnoteRef :: a -> a

instance HasFootnoteRef Builder where
  footnoteRef x = "<sup>" <> x <> "</sup>"

instance (HasFootnoteRef i, Monoid i)
        => HasFootnoteRef (WithSourceMap i) where
  footnoteRef x = (footnoteRef <$> x) <* addName "footnoteRef"
