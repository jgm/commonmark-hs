{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.HardLineBreaks
  ( hardLineBreaksSpec )
where
import Commonmark.Types
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.TokParsers
import Commonmark.Tokens

hardLineBreaksSpec :: (Monad m, IsBlock il bl, IsInline il)
                   => SyntaxSpec m il bl
hardLineBreaksSpec = mempty
  { syntaxInlineParsers = [ hardLineBreakParser ]
  }

hardLineBreakParser :: (Monad m, IsInline a) => InlineParser m a
hardLineBreakParser = lineBreak <$ satisfyTok (hasType LineEnd)

