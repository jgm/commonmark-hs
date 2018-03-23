{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.Autolink
  ( autolinkSpec )
where
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.Util
import Text.Parsec
import Lucid
import Data.Text (Text)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

autolinkSpec :: (Monad m, IsBlock il bl, IsInline il)
             => SyntaxSpec m il bl
autolinkSpec = SyntaxSpec
  { syntaxBlockSpecs = []
  , syntaxBracketedSpecs = []
  , syntaxFormattingSpecs = []
  , syntaxInlineParsers = [parseAutolink]
  }

parseAutolink :: (Monad m, IsInline a) => InlineParser m a
parseAutolink = fail "TODO"
