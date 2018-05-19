{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.Footnote
  ( footnoteSpec )
where
import Commonmark.Types
import Commonmark.Syntax
-- import Commonmark.Inlines
-- import Data.Semigroup (Semigroup(..))

footnoteSpec :: (Monad m, IsBlock il bl, IsInline il)
             => SyntaxSpec m il bl
footnoteSpec = SyntaxSpec
  { syntaxBlockSpecs = []
  , syntaxBracketedSpecs = []
  , syntaxFormattingSpecs = []
  , syntaxInlineParsers = []
  }
