module Commonmark.Parser
    ( commonmark
    , commonmarkWith
    , parseCommonmarkWith
    , module Commonmark.Tokens
    , module Commonmark.Types
    , module Commonmark.Syntax
    -- * Exported from "Text.Parsec.Error"
    , ParseError
    ) where

import           Commonmark.Blocks
import           Commonmark.Inlines
import           Commonmark.Tokens
import           Commonmark.Types
import           Commonmark.Syntax (SyntaxSpec(..), defaultSyntaxSpec)
import           Text.Parsec.Error (ParseError)
import           Data.Functor.Identity   (runIdentity)
import           Data.Text (Text)

-- | Parse a commonmark document using the core syntax
-- elements. Return mempty if there is a parse error (not expected
-- for commonmark, since every string is a valid commonmark document).
-- To produce HTML, instantiate 'bl' with @'Html' ()@ (see
-- 'Commonmark.Html'.
-- If you want to add syntax extensions or run the parser in a
-- monadic context, use 'commonmarkWith'.
-- If you want to operate on tokenized input, use 'parseCommonmarkWith'.
commonmark :: IsBlock il bl
           => String      -- ^ Name or path of input
           -> Text        -- ^ Commonmark text input
           -> bl          -- ^ Result
commonmark sourcename =
 either mempty id .
 runIdentity .
 parseCommonmarkWith defaultSyntaxSpec .
 tokenize sourcename

-- | Like 'commonmark', but allows specifying a custom syntax
-- and a monadic context (since some syntax extensions may
-- only be defined in certain monads, e.g. an extension for
-- include files may require IO).
commonmarkWith :: (Monad m, IsBlock il bl, IsInline il)
               => SyntaxSpec m il bl       -- ^ Defines syntax
               -> String                   -- ^ Name or path of input
               -> Text                     -- ^ Commonmark text input
               -> m bl                     -- ^ Result
commonmarkWith syntax sourcename =
 fmap (either mempty id) .
 parseCommonmarkWith syntax .
 tokenize sourcename

-- | Parse a tokenized commonmark document using specified
-- syntax elements.  Use 'tokenize' to convert 'Text' into ['Tok'].
parseCommonmarkWith :: (Monad m, IsBlock il bl, IsInline il)
                    => SyntaxSpec m il bl -- ^ Defines syntax
                    -> [Tok] -- ^ Tokenized commonmark input
                    -> m (Either ParseError bl)  -- ^ Result or error
parseCommonmarkWith syntax =
    mkBlockParser (syntaxBlockSpecs syntax)
      (syntaxFinalParsers syntax)
      (mkInlineParser (syntaxBracketedSpecs syntax)
                      (syntaxFormattingSpecs syntax)
                      (syntaxInlineParsers syntax)
                      (syntaxAttributeParsers syntax))
      (syntaxAttributeParsers syntax)
