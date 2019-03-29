{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.Attributes
  ( Attributes(..)
  , headerAttributesSpec
  )
where
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.Util
import Commonmark.Html (escapeHtml)
import Text.Parsec
import Data.Text (Text)
import Data.Semigroup (Semigroup(..))

headerAttributesSpec :: (Monad m, IsBlock il bl, IsInline il, HasAttributes bl)
                     => SyntaxSpec m il bl
headerAttributes = SyntaxSpec
  { syntaxBlockSpecs = []
  , syntaxBracketedSpecs = []
  , syntaxFormattingSpecs = []
  , syntaxInlineParsers = []
  , syntaxFinalParsers = []
  }

type Attributes = [(Text, Text)]
