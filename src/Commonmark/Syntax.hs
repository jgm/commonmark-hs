module Commonmark.Syntax
  ( SyntaxSpec(..)
  , defaultSyntaxSpec
  )
where

import Commonmark.Types
import Commonmark.Blocks
import Commonmark.Inlines
import Data.Semigroup

-- | A 'SyntaxSpec' defines a basic collection of syntax
-- elements or an extension.  'SyntaxSpec's can be composed
-- using monoidal 'mappend'.
data SyntaxSpec m il bl = SyntaxSpec
     { syntaxBlockSpecs      :: [BlockSpec m il bl]
        -- ^ Defines block structure
     , syntaxBracketedSpecs  :: [BracketedSpec il]
        -- ^ Defines bracketed inline containers (inline, image)
     , syntaxFormattingSpecs :: [FormattingSpec il]
        -- ^ Defines formatted inline containers (strong, emph)
     , syntaxInlineParsers   :: [InlineParser m il]
        -- ^ Defines inline elements that don't contain inlines
     }

instance Semigroup (SyntaxSpec m il bl) where
  SyntaxSpec bl1 br1 fo1 il1 <> SyntaxSpec bl2 br2 fo2 il2
    = SyntaxSpec (bl1 <> bl2) (br1 <> br2) (fo1 <> fo2) (il1 <> il2)
instance Monoid (SyntaxSpec m il bl) where
  mempty = SyntaxSpec mempty mempty mempty mempty
  mappend = (<>)

-- | Standard commonmark syntax.
defaultSyntaxSpec :: (Monad m, IsBlock il bl, IsInline il)
                  => SyntaxSpec m il bl
defaultSyntaxSpec = SyntaxSpec
  { syntaxBlockSpecs      = defaultBlockSpecs
  , syntaxBracketedSpecs  = defaultBracketedSpecs
  , syntaxFormattingSpecs = defaultFormattingSpecs
  , syntaxInlineParsers   = defaultInlineParsers
  }
