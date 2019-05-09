{-# LANGUAGE CPP #-}
module Commonmark.Syntax
  ( SyntaxSpec(..)
  , defaultSyntaxSpec
  )
where

import Text.Parsec (Parsec)
import Control.Monad (mzero)
import Data.Monoid (Alt(..))
import Data.Text (Text)
import Commonmark.Tokens (Tok)
import Commonmark.Types
import Commonmark.Blocks
import Commonmark.Inlines
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

-- | A 'SyntaxSpec' defines a basic collection of syntax
-- elements or an extension.  'SyntaxSpec's can be composed
-- using monoidal 'mappend'.
data SyntaxSpec m il bl = SyntaxSpec
     { syntaxBlockSpecs      :: [BlockSpec m il bl]
        -- ^ Defines block structure
     , syntaxBracketedSpecs  :: [BracketedSpec il]
        -- ^ Defines bracketed inline containers (inli, image)
     , syntaxFormattingSpecs :: [FormattingSpec il]
        -- ^ Defines formatted inline containers (strong, emph)
     , syntaxInlineParsers   :: [InlineParser m il]
        -- ^ Defines inline elements that don't contain inlines
     , syntaxFinalParsers    :: [BlockParser m il bl bl]
        -- ^ Run at the end of document, e.g. to collect footnotes
     , syntaxReferenceLinkParser
             :: Alt Maybe (Parsec [Tok] () ((SourceRange, Text), LinkInfo))
       -- ^ Parse link reference definition
     }

instance Semigroup (SyntaxSpec m il bl) where
  SyntaxSpec bl1 br1 fo1 il1 fp1 ld1 <> SyntaxSpec bl2 br2 fo2 il2 fp2 ld2
    = SyntaxSpec (bl1 <> bl2) (br1 <> br2) (fo1 <> fo2) (il1 <> il2)
                 (fp1 <> fp2) (ld1 <> ld2)
instance Monoid (SyntaxSpec m il bl) where
  mempty = SyntaxSpec mempty mempty mempty mempty mempty (Alt Nothing)
  mappend = (<>)

-- | Standard commonmark syntax.
defaultSyntaxSpec :: (Monad m, IsBlock il bl, IsInline il)
                  => SyntaxSpec m il bl
defaultSyntaxSpec = SyntaxSpec
  { syntaxBlockSpecs          = defaultBlockSpecs
  , syntaxBracketedSpecs      = defaultBracketedSpecs
  , syntaxFormattingSpecs     = defaultFormattingSpecs
  , syntaxInlineParsers       = defaultInlineParsers
  , syntaxFinalParsers        = []
  , syntaxReferenceLinkParser = Alt (Just (linkReferenceDef mzero))
  }
