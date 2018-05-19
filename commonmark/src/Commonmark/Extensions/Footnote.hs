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
import Control.Monad (mzero)
import Data.Semigroup (Semigroup(..))
import Data.Text.Lazy.Builder (Builder)

footnoteSpec :: (Monad m, IsBlock il bl, IsInline il, HasFootnoteRef il)
             => SyntaxSpec m il bl
footnoteSpec = SyntaxSpec
  { syntaxBlockSpecs = [footnoteBlockSpec]
  , syntaxBracketedSpecs = []
  , syntaxFormattingSpecs = []
  , syntaxInlineParsers = [pFootnoteRef]
  }

footnoteBlockSpec :: BlockSpec m il bl
footnoteBlockSpec = BlockSpec
     { blockType           = "Footnote"
     , blockStart          = mzero
     , blockCanContain     = const True
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \node -> mzero
     , blockConstructor    = \node -> mzero
     , blockFinalize       = \node node2 -> mzero
     }

pFootnoteRef :: (Monad m, HasFootnoteRef a) => InlineParser m a
pFootnoteRef = fail "undefined"

class HasFootnoteRef a where
  footnoteRef :: a -> a

instance HasFootnoteRef Builder where
  footnoteRef x = "<sup>" <> x <> "</sup>"

instance (HasFootnoteRef i, Monoid i)
        => HasFootnoteRef (WithSourceMap i) where
  footnoteRef x = (footnoteRef <$> x) <* addName "footnoteRef"
