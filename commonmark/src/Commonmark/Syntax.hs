{-# LANGUAGE RankNTypes #-}
module Commonmark.Syntax
  ( SyntaxSpec(..)
  , defaultSyntaxSpec
  )
where

import Text.Parsec (ParsecT)
import Commonmark.Tokens (Tok)
import Commonmark.Types
import Commonmark.Blocks
import Commonmark.Inlines

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
     , syntaxAttributeParsers
             :: forall u m1 . Monad m1 => [ParsecT [Tok] u m1 Attributes]
       -- ^ Parse attributes
     }

instance Semigroup (SyntaxSpec m il bl) where
  SyntaxSpec bl1 br1 fo1 il1 fp1 ap1 <> SyntaxSpec bl2 br2 fo2 il2 fp2 ap2
    = SyntaxSpec (removeDuplicateBlockSpecs $ bl1 <> bl2)
                 (br1 <> br2) (fo1 <> fo2) (il1 <> il2)
                 (fp1 <> fp2) (ap1 <> ap2)
instance Monoid (SyntaxSpec m il bl) where
  mempty = SyntaxSpec mempty mempty mempty mempty mempty mempty
  mappend = (<>)

-- | When several specs have the same 'blockType', keep the first
-- one, but put it in the position of the last one.  This way an
-- extension's spec replaces a default spec in place, rather than
-- moving ahead of other extensions' specs (#180).
removeDuplicateBlockSpecs :: [BlockSpec m il bl] -> [BlockSpec m il bl]
removeDuplicateBlockSpecs []     = []
removeDuplicateBlockSpecs (b:bs) =
  case break ((== blockType b) . blockType) bs of
    (_, [])      -> b : removeDuplicateBlockSpecs bs
    (xs, _:ys)   -> removeDuplicateBlockSpecs (xs ++ b : ys)

-- | Standard commonmark syntax.
defaultSyntaxSpec :: (Monad m, IsBlock il bl, IsInline il)
                  => SyntaxSpec m il bl
defaultSyntaxSpec = SyntaxSpec
  { syntaxBlockSpecs          = defaultBlockSpecs
  , syntaxBracketedSpecs      = defaultBracketedSpecs
  , syntaxFormattingSpecs     = defaultFormattingSpecs
  , syntaxInlineParsers       = [defaultInlineParser]
  , syntaxFinalParsers        = []
  , syntaxAttributeParsers    = []
  }
